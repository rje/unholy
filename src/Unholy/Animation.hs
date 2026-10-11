{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
-- cgltf decodes accessors. Haskell owns the hierarchy, clock and skinning.
module Unholy.Animation (Rig(..), RigMesh(..), Node(..), Skin(..), Clip(..), Channel(..), loadRig, validateAnimator, advanceAnimators, posedMeshes, sampleGlobals) where

import Unholy.Types (Animator(..),World(..),finite)
import qualified Unholy.Renderer.GLTF as F
import qualified Data.Aeson as A
import Data.Aeson ((.:),(.:?),(.!=))
import qualified Data.Aeson.Types as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Linear hiding (finite)
import Foreign
import Foreign.C
import Control.Exception (bracket)
import Control.Monad (unless,forM,forM_)
import Data.Fixed (mod')

data Node = Node { nodeParent :: !Int, nodeMatrix :: !(M44 Float), nodeTranslation :: !(V3 Float), nodeRotation :: !(Quaternion Float), nodeScale :: !(V3 Float), matrixNode :: !Bool } deriving Show
data Skin = Skin { skinJoints :: ![Int], inverseBinds :: ![M44 Float] } deriving Show
data RigMesh = RigMesh { bindVertices :: !(VS.Vector Float), triangleIndices :: !(VS.Vector Word16), vertexJoints :: !(VS.Vector (V4 Int)), vertexWeights :: !(VS.Vector (V4 Float)), meshSkin :: !Int } deriving Show
data Channel = Channel { channelNode :: !Int, channelPath :: !String, channelStep :: !Bool, keyTimes :: !(VS.Vector Float), keyValues :: !(VS.Vector (V4 Float)) } deriving Show
data Clip = Clip { clipDuration :: !Float, clipChannels :: ![Channel] } deriving Show
data Rig = Rig { rigNodes :: !(V.Vector Node), rigSkins :: !(V.Vector Skin), rigMeshes :: ![RigMesh], rigClips :: !(M.Map String Clip) } deriving Show
data RawMesh = RawMesh Int Int Int Int Int (Maybe Int) Int
data RawChannel = RawChannel Int String Bool Int Int
data RawClip = RawClip String [RawChannel]
data Document = Document [Node] [(Int,Int)] [([Int],Maybe Int)] [RawMesh] [RawClip]

foreign import ccall unsafe "uh_gltf_load" cLoad :: CString -> IO (Ptr ())
foreign import ccall unsafe "uh_gltf_free" cFree :: Ptr () -> IO ()
foreign import ccall unsafe "uh_gltf_accessor" cAccessor :: Ptr () -> CInt -> Ptr CInt -> Ptr Float -> IO CInt

loadRig :: FilePath -> IO Rig
loadRig path=do
  _<-F.loadAnimatedMaterials path
  value<-F.readDocument path >>= maybe (bad "Animation assets must be glTF/GLB") pure
  Document nodes links skins meshes clips<-either bad pure (A.parseEither document value)
  unless (not (null nodes) && length nodes<=512 && not (null skins) && not (null meshes)) (bad "Empty or oversized rig")
  let parents=IM.fromList links
      ns=V.fromList [n {nodeParent=IM.findWithDefault (-1) i parents} | (i,n)<-zip [0..] nodes]
      validNode i=i>=0 && i<V.length ns
      chain seen i | i<0=Just ()
                   | not (validNode i) || i `elem` seen=Nothing
                   | otherwise=chain (i:seen) (nodeParent (ns V.! i))
  unless (length links==IM.size parents && all (\i->chain [] i==Just ()) [0..V.length ns-1]) (bad "Invalid rig hierarchy")
  withCString path $ \name->bracket (cLoad name) (\p->unless (p==nullPtr) (cFree p)) $ \p->do
    unless (p/=nullPtr) (bad "cgltf rejected animation data")
    let readArray index size=allocaArray 2 $ \counts->do
          ok<-cAccessor p (fromIntegral index) counts nullPtr
          unless (ok/=0) (bad "Invalid animation accessor")
          count<-fromIntegral <$> peekElemOff counts 0
          components<-fromIntegral <$> peekElemOff counts 1
          unless (ok/=0 && count>0 && components==size && count*size<=1000000) (bad "Invalid animation accessor layout")
          ptr<-mallocForeignPtrArray (count*size)
          copied<-withForeignPtr ptr (cAccessor p (fromIntegral index) counts)
          let result=VS.unsafeFromForeignPtr0 ptr (count*size)
          unless (copied/=0 && VS.all finite result) (bad "Invalid or nonfinite animation accessor")
          pure result
    ss<-forM skins $ \(joints,index)->do
      unless (not (null joints) && all validNode joints) (bad "Invalid skin joints")
      matrices<-maybe (pure (replicate (length joints) identity)) (\a->do
        raw<-readArray a 16
        unless (VS.length raw==length joints*16) (bad "Inverse bind count mismatch")
        pure [matrix (VS.toList (VS.slice (i*16) 16 raw)) | i<-[0..length joints-1]]) index
      unless (all (\m->abs (det44 m)>1e-8) matrices) (bad "Singular inverse bind matrix")
      pure (Skin joints matrices)
    ms<-forM meshes $ \(RawMesh pos normal uv joints weights indices skin)->do
      unless (skin>=0 && skin<length ss) (bad "Mesh requires a valid skin")
      ps<-readArray pos 3;ns'<-readArray normal 3;uvs<-readArray uv 2;js<-readArray joints 4;ws<-readArray weights 4
      let count=VS.length ps `div` 3
      unless (count<=65535 && VS.length ns'==count*3 && VS.length uvs==count*2 && VS.length js==count*4 && VS.length ws==count*4) (bad "Skin attribute counts disagree or exceed 65535 vertices")
      is<-maybe (pure (VS.generate count fromIntegral)) (\a->readArray a 1) indices
      unless (VS.length is `mod` 3==0 && VS.all (integerIn count) is && VS.all (integerIn (length (skinJoints (ss!!skin)))) js) (bad "Invalid triangle/joint indices")
      let weights4=VS.generate count (\i->v4At ws (i*4))
      unless (VS.all (\v->all (>=0) v && abs (sum v-1)<0.005) weights4 && VS.all (\i->norm (v3At ns' (i*3))>1e-6) (VS.generate count id)) (bad "Invalid normals or skin weights")
      let raw=VS.generate (count*8) $ \i->let (v,c)=i `divMod` 8 in if c<3 then ps VS.! (v*3+c) else if c<5 then uvs VS.! (v*2+c-3) else ns' VS.! (v*3+c-5)
      pure (RigMesh raw (VS.map round is) (VS.generate count (\i->fmap round (v4At js (i*4)))) (VS.map (\v->v ^/ sum v) weights4) skin)
    cs<-forM clips $ \(RawClip name channels)->do
      chs<-forM channels $ \(RawChannel node path' step input output)->do
        unless (validNode node && not (matrixNode (ns V.! node))) (bad "Animation targets require a TRS node")
        ts<-readArray input 1
        vs<-readArray output (if path'=="rotation" then 4 else 3)
        unless (VS.head ts>=0 && and (zipWith (<) (VS.toList ts) (drop 1 (VS.toList ts))) && VS.length vs==VS.length ts*(if path'=="rotation" then 4 else 3)) (bad "Animation keys must be strictly ordered with matching values")
        let values=VS.generate (VS.length ts) (\i->if path'=="rotation" then v4At vs (i*4) else let V3 x y z=v3At vs (i*3) in V4 x y z 0)
        unless (path'/="rotation" || VS.all (\v->abs (norm v-1)<0.005) values) (bad "Rotation keys must be unit quaternions")
        unless (path'/="scale" || VS.all (\(V4 x y z _)->all (>0) [x,y,z]) values) (bad "Animated scale must stay positive")
        pure (Channel node path' step ts values)
      unless (not (null chs) && length [(channelNode c,channelPath c) | c<-chs]==M.size (M.fromList [((channelNode c,channelPath c),()) | c<-chs])) (bad "Empty clip or duplicate animation channel")
      pure (name,Clip (maximum (map (VS.last . keyTimes) chs)) chs)
    unless (length cs==M.size (M.fromList cs)) (bad "Clip names must be unique")
    pure (Rig ns (V.fromList ss) ms (M.fromList cs))
  where bad msg=ioError (userError (path++": "++msg))
        integerIn count n=n>=0 && n<fromIntegral count && n==fromIntegral (round n::Int)

document :: A.Value -> A.Parser Document
document=A.withObject "animated glTF" $ \o->do
  rawNodes<-o .: "nodes";rawMeshes<-o .: "meshes";rawSkins<-o .: "skins";rawAccessors<-o .: "accessors"
  nodes<-mapM node rawNodes
  children<-mapM (A.withObject "node" (\n->n .:? "children" .!= [])) rawNodes
  instances<-fmap concat $ forM rawNodes $ A.withObject "node" $ \n->do
    m<-n .:? "mesh"
    case m of
      Nothing->pure []
      Just i->do
        skin<-n .: "skin"
        unless (i>=0 && i<length rawMeshes) (fail "Invalid mesh node")
        A.withObject "mesh" (\mesh->do
          ps<-mesh .: "primitives"
          mapM (primitive skin) ps) (rawMeshes!!i)
  skins<-mapM (A.withObject "skin" (\s->(,) <$> s .: "joints" <*> s .:? "inverseBindMatrices")) rawSkins
  -- The material-image integration still enters raylib's model importer, whose
  -- geometry reader needs base buffer views and these concrete attribute types.
  forM_ instances $ \(RawMesh p n uv j w indices _)->do
    let check :: Int -> String -> [Int] -> A.Parser ()
        check index layout allowed=do
          unless (index>=0 && index<length rawAccessors) (fail "Invalid geometry accessor")
          A.withObject "geometry accessor" (\a->do
            view<-a .:? "bufferView" :: A.Parser (Maybe Int)
            sparse<-a .:? "sparse" :: A.Parser (Maybe A.Value)
            component<-a .: "componentType";kind<-a .: "type"
            unless (view/=Nothing && sparse==Nothing && kind==layout && component `elem` allowed) (fail "Skin geometry requires nonsparse base views with supported attribute types")) (rawAccessors!!index)
    check p ("VEC3"::String) [5126::Int];check n "VEC3" [5126];check uv "VEC2" [5126]
    check j "VEC4" [5121,5123];check w "VEC4" [5126]
    mapM_ (\index->check index "SCALAR" [5121,5123,5125]) indices
  animations<-o .:? "animations" .!= []
  clips<-forM (zip [0::Int ..] animations) $ \(i,value)->A.withObject "animation" (\a->do
    name<-a .:? "name" .!= ("clip-"++show i)
    samplers<-a .: "samplers";channels<-a .: "channels"
    parsed<-forM channels $ A.withObject "channel" $ \c->do
      sampler<-c .: "sampler";target<-c .: "target"
      unless (sampler>=0 && sampler<length samplers) (fail "Invalid animation sampler")
      (n,path)<-A.withObject "target" (\t->(,) <$> t .: "node" <*> t .: "path") target
      unless (path `elem` ["translation","rotation","scale"]) (fail "Morph animation is not supported")
      A.withObject "sampler" (\s->do
        mode<-s .:? "interpolation" .!= ("LINEAR"::String)
        unless (mode `elem` ["LINEAR","STEP"]) (fail "Only LINEAR and STEP animation are supported")
        RawChannel n path (mode=="STEP") <$> s .: "input" <*> s .: "output") (samplers!!sampler)
    pure (RawClip name parsed)) value
  pure (Document nodes [(child,parent) | (parent,cs)<-zip [0..] children,child<-cs] skins instances clips)
  where
    node=A.withObject "node" $ \n->do
      t<-n .:? "translation" .!= [0,0,0];r<-n .:? "rotation" .!= [0,0,0,1];s<-n .:? "scale" .!= [1,1,1];m<-n .:? "matrix"
      unless (length t==3 && length r==4 && length s==3 && all finite (t++r++s++maybe [] id m) && all (>0) s && abs (norm (v4 r)-1)<0.005 && maybe True ((==16).length) m) (fail "Invalid node transform")
      let q=quaternion (v4 r);translation=v3 t;scaling=v3 s
          transform=maybe (trs translation q scaling) matrix m
      unless (abs (det44 transform)>1e-8) (fail "Singular node transform")
      pure (Node (-1) transform translation q scaling (maybe False (const True) m))
    primitive skin=A.withObject "primitive" $ \p->do
      attrs<-p .: "attributes"
      keys<-A.withObject "attributes" (pure . map K.toString . KM.keys) attrs
      unless (all (`elem` ["POSITION","NORMAL","TEXCOORD_0","JOINTS_0","WEIGHTS_0"]) keys) (fail "Unsupported skin attribute")
      extraJoints<-A.withObject "attributes" (\a->a .:? "JOINTS_1" :: A.Parser (Maybe Int)) attrs
      extraWeights<-A.withObject "attributes" (\a->a .:? "WEIGHTS_1" :: A.Parser (Maybe Int)) attrs
      unless (extraJoints==Nothing && extraWeights==Nothing) (fail "Only four skin influences are supported")
      let get k=A.withObject "attributes" (.: k) attrs
      RawMesh <$> get "POSITION" <*> get "NORMAL" <*> get "TEXCOORD_0" <*> get "JOINTS_0" <*> get "WEIGHTS_0" <*> p .:? "indices" <*> pure skin

matrix :: [Float] -> M44 Float
matrix xs=transpose (V4 (row 0) (row 4) (row 8) (row 12)) where row i=v4 (take 4 (drop i xs))
v3 :: [Float] -> V3 Float
v3 [x,y,z]=V3 x y z
v3 _=V3 0 0 0
v4 :: [Float] -> V4 Float
v4 [x,y,z,w]=V4 x y z w
v4 _=V4 0 0 0 1
v3At :: VS.Vector Float -> Int -> V3 Float
v3At a i=V3 (a VS.! i) (a VS.! (i+1)) (a VS.! (i+2))
v4At :: VS.Vector Float -> Int -> V4 Float
v4At a i=V4 (a VS.! i) (a VS.! (i+1)) (a VS.! (i+2)) (a VS.! (i+3))
quaternion :: V4 Float -> Quaternion Float
quaternion (V4 x y z w)=normalize (Quaternion w (V3 x y z))
trs :: V3 Float -> Quaternion Float -> V3 Float -> M44 Float
trs t q (V3 x y z)=mkTransformation q t !*! V4 (V4 x 0 0 0) (V4 0 y 0 0) (V4 0 0 z 0) (V4 0 0 0 1)

validateAnimator :: Rig -> Animator -> Either String ()
validateAnimator rig a | null (animationClip a) || M.member (animationClip a) (rigClips rig)=Right ()
                       | otherwise=Left ("Unknown animation clip: "++animationClip a)
advanceAnimators :: Float -> World -> World
advanceAnimators dt w=w {animators=IM.map (\a->if animationPlaying a then a {animationTime=animationTime a+dt*animationRate a} else a) (animators w)}

sampleGlobals :: Rig -> Animator -> V.Vector (M44 Float)
sampleGlobals rig a=V.imap (\i _->global i) locals
  where
    clip=M.lookup (animationClip a) (rigClips rig)
    time=maybe 0 (\c->if animationLoop a && clipDuration c>0 then animationTime a `mod'` clipDuration c else min (animationTime a) (clipDuration c)) clip
    channels=maybe [] clipChannels clip
    changes=foldl (\table c->IM.insertWith (++) (channelNode c) [(channelPath c,sample time c)] table) IM.empty channels
    locals=V.imap local (rigNodes rig)
    local i n=case IM.lookup i changes of
      Nothing->nodeMatrix n
      Just values->
        let vec path fallback=maybe fallback (\(V4 x y z _)->V3 x y z) (lookup path values)
            q=maybe (nodeRotation n) quaternion (lookup "rotation" values)
        in trs (vec "translation" (nodeTranslation n)) q (vec "scale" (nodeScale n))
    global i=let parent=nodeParent (rigNodes rig V.! i) in if parent<0 then locals V.! i else global parent !*! (locals V.! i)
    sample t c
      | VS.length (keyTimes c)==1 || t<=VS.head (keyTimes c)=VS.head (keyValues c)
      | t>=VS.last (keyTimes c)=VS.last (keyValues c)
      | otherwise=let index=search 0 (VS.length (keyTimes c)-1);start=keyTimes c VS.! index;end=keyTimes c VS.! (index+1)
                      alpha=(t-start)/(end-start);left=keyValues c VS.! index;right=keyValues c VS.! (index+1)
                  in if channelStep c then left else if channelPath c=="rotation" then let Quaternion w (V3 x y z)=slerp (quaternion left) (quaternion right) alpha in V4 x y z w else lerp alpha left right
      where search lo hi | hi-lo<=1=lo
                         | keyTimes c VS.! mid<=t=search mid hi
                         | otherwise=search lo mid
              where mid=(lo+hi) `div` 2

-- Vertex snapshots feed drawing, shadowing, culling and picking alike. A skinned
-- mesh node transform is ignored by glTF; the entity transform is applied later.
posedMeshes :: Rig -> Animator -> [VS.Vector Float]
posedMeshes rig a=map deform (rigMeshes rig)
  where
    globals=sampleGlobals rig a
    palettes=V.map (\s->V.fromList (zipWith (\joint bind->globals V.! joint !*! bind) (skinJoints s) (inverseBinds s))) (rigSkins rig)
    normalPalettes=V.map (V.map (transpose . inv44)) palettes
    deform mesh=VS.concatMap vertexData (VS.generate (VS.length (bindVertices mesh) `div` 8) id)
      where
        vertexData vertex=
         let
            palette=palettes V.! meshSkin mesh
            normals=normalPalettes V.! meshSkin mesh
            js=vertexJoints mesh VS.! vertex;ws=vertexWeights mesh VS.! vertex
            skin=foldl (^+^) (pure (pure 0)) (zipWith (\joint weight->fmap (fmap (*weight)) (palette V.! joint)) (toList4 js) (toList4 ws))
            pos=v3At (bindVertices mesh) (vertex*8)
            normal=v3At (bindVertices mesh) (vertex*8+5)
            point=skin !* point4 pos 1
            direction=normalize (foldl (^+^) (pure 0) (zipWith (\joint weight->weight *^ xyz (normals V.! joint !* point4 normal 0)) (toList4 js) (toList4 ws)))
         in VS.fromList (toList3 (xyz point)++[bindVertices mesh VS.! (vertex*8+3),bindVertices mesh VS.! (vertex*8+4)]++toList3 direction)
    point4 (V3 x y z) w=V4 x y z w
    xyz (V4 x y z _)=V3 x y z
    toList3 (V3 x y z)=[x,y,z]
    toList4 (V4 x y z w)=[x,y,z,w]
