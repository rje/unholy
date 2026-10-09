module HJ.Types where

import qualified Data.IntMap.Strict as IM
import Data.List (nub)

type Entity = Int
data V3 = V3 !Float !Float !Float deriving (Eq, Show, Read)
data Transform = Transform { position :: !V3, rotation :: !V3, scale :: !V3 }
  deriving (Eq, Show, Read)
data Material = Material { albedo :: !V3, metallic :: !Float, roughness :: !Float, emission :: !Float }
  deriving (Eq, Show, Read)
data Mesh = Mesh { geometry :: String, texture :: FilePath } deriving (Eq, Show, Read)
data Light = Light { lightColor :: !V3, intensity :: !Float } deriving (Eq, Show, Read)
data BodyShape = Box | Sphere deriving (Eq,Show,Read)
data Body = Body { bodyShape :: !BodyShape, mass :: !Float, velocity :: !V3, restitution :: !Float, friction :: !Float, grounded :: !Bool, kinematic :: !Bool }
  deriving (Eq,Show,Read)
defaultBody :: Body
defaultBody = Body Box 0 (V3 0 0 0) 0.15 0.6 False False
data Camera = Camera { eye :: !V3, target :: !V3, fov :: !Float } deriving (Eq, Show, Read)
data Environment = Environment { sunDirection :: !V3, sunColor :: !V3, sunPower :: !Float, ambient :: !Float, exposure :: !Float, skyTop :: !V3, skyBottom :: !V3, fogDensity :: !Float, sunShadows :: !Bool }
  deriving (Eq, Show, Read)

-- Each component has its own typed store. An entity is a stable, monotonic ID.
-- Systems join only the tables they need; deleting removes every component.
data World = World
  { nextId :: !Entity
  , names :: !(IM.IntMap String)
  , transforms :: !(IM.IntMap Transform)
  , meshes :: !(IM.IntMap Mesh)
  , materials :: !(IM.IntMap Material)
  , scripts :: !(IM.IntMap FilePath)
  , lights :: !(IM.IntMap Light)
  , bodies :: !(IM.IntMap Body)
  } deriving (Eq, Show, Read)

data Project = Project { projectName :: String, world :: !World, camera :: !Camera, environment :: !Environment }
  deriving (Eq, Show, Read)

identity :: Transform
identity = Transform (V3 0 0 0) (V3 0 0 0) (V3 1 1 1)
defaultMaterial :: Material
defaultMaterial = Material (V3 0.72 0.78 0.85) 0 0.4 0
defaultCamera :: Camera
defaultCamera = Camera (V3 10 7 11) (V3 0 1 0) 45
defaultEnvironment :: Environment
defaultEnvironment = Environment (V3 (-0.5) (-1) (-0.4)) (V3 1 0.92 0.82) 3 0.12 1 (V3 0.067 0.098 0.145) (V3 0.125 0.173 0.208) 0 False
emptyWorld :: World
emptyWorld = World 1 IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty IM.empty
emptyProject :: String -> Project
emptyProject n = Project n emptyWorld defaultCamera defaultEnvironment

spawn :: String -> World -> (Entity, World)
spawn n w = (e, w { nextId = e+1, names = IM.insert e n (names w), transforms = IM.insert e identity (transforms w) })
  where e = nextId w

spawnMesh :: String -> String -> World -> (Entity, World)
spawnMesh n g w = (e, w' { meshes = IM.insert e (Mesh g "") (meshes w'), materials = IM.insert e defaultMaterial (materials w') })
  where (e,w') = spawn n w

destroy :: Entity -> World -> World
destroy e w = w { names = IM.delete e (names w), transforms = IM.delete e (transforms w), meshes = IM.delete e (meshes w), materials = IM.delete e (materials w), scripts = IM.delete e (scripts w), lights = IM.delete e (lights w), bodies = IM.delete e (bodies w) }

duplicate :: Entity -> World -> (Entity, World)
duplicate e w = (n, w' { transforms = copy transforms w', meshes = copy meshes w', materials = copy materials w', scripts = copy scripts w', lights = copy lights w', bodies = copy bodies w' })
  where
    (n,w') = spawn (IM.findWithDefault "Entity" e (names w) ++ " copy") w
    copy field base = maybe (field base) (\v -> IM.insert n v (field base)) (IM.lookup e (field w))

renderables :: World -> [(Entity, Transform, Mesh, Material)]
renderables w = [(e,t,m,mat) | (e,m) <- IM.toAscList (meshes w), Just t <- [IM.lookup e (transforms w)], Just mat <- [IM.lookup e (materials w)]]

validateWorld :: World -> Either String ()
validateWorld w
  | any (<=0) ids = Left "Entity IDs must be positive"
  | nextId w <= maximum (0:ids) = Left "nextId must be greater than all entity IDs"
  | any (`IM.notMember` names w) componentIds = Left "A component refers to a missing entity"
  | any (`IM.notMember` transforms w) ids = Left "Every entity needs a transform"
  | any (`IM.notMember` materials w) (IM.keys (meshes w)) = Left "Every mesh needs a material"
  | any badTransform (IM.elems (transforms w)) = Left "Transforms must be finite with positive scale"
  | any badMaterial (IM.elems (materials w)) = Left "Invalid PBR material values"
  | IM.size (lights w) > 8 = Left "This renderer supports at most eight point lights"
  | any (\l -> not (validV (lightColor l)) || not (finite (intensity l)) || intensity l < 0) (IM.elems (lights w)) = Left "Invalid point light"
  | any badBody (IM.elems (bodies w)) = Left "Invalid rigid body"
  | otherwise = Right ()
  where
    ids = IM.keys (names w)
    componentIds = nub (concat [IM.keys (transforms w),IM.keys (meshes w),IM.keys (materials w),IM.keys (scripts w),IM.keys (lights w),IM.keys (bodies w)])
    badTransform (Transform p r s) = not (all validV [p,r,s]) || any (<=0) (vlist s)
    badMaterial (Material a m r e) = not (validV a) || any (\x -> x<0 || x>1) (vlist a) || not (all finite [m,r,e]) || m<0 || m>1 || r<0.04 || r>1 || e<0
    badBody b = not (validV (velocity b) && all finite [mass b,restitution b,friction b]) || mass b<0 || restitution b<0 || restitution b>1 || friction b<0 || (kinematic b && mass b/=0)

finite :: Float -> Bool
finite x = not (isNaN x || isInfinite x)
validV :: V3 -> Bool
validV = all finite . vlist
vlist :: V3 -> [Float]
vlist (V3 x y z) = [x,y,z]
fromListV :: [Float] -> V3
fromListV [x,y,z] = V3 x y z
fromListV _ = V3 0 0 0

componentData :: Transform -> Material -> [Float]
componentData t m = concatMap vlist [position t, rotation t, scale t, albedo m] ++ [metallic m,roughness m,emission m]

updateComponentData :: [Float] -> Transform -> Material -> (Transform,Material)
updateComponentData xs t m
  | length xs /= 15 || not (all finite xs) = (t,m)
  | otherwise = (Transform (v 0) (v 3) (fromListV (map (max 0.001) (take 3 (drop 6 xs)))), Material (fromListV (map unit (take 3 (drop 9 xs)))) (unit (xs!!12)) (max 0.04 (unit (xs!!13))) (max 0 (xs!!14)))
  where v i = fromListV (take 3 (drop i xs)); unit = max 0 . min 1
