module Unholy.Editor (runEditor, runPlayer) where

import Unholy.Types
import Unholy.Transform (setTransform,relative)
import Unholy.Prefab
import qualified Unholy.Gizmo as Gizmo
import qualified Unholy.Renderer.Camera as C
import qualified Linear as L
import Unholy.Project
import qualified Unholy.Native as N
import Unholy.Script
import qualified Data.IntMap.Strict as IM
import Control.Monad (foldM, forM_, when)
import System.Directory
import System.FilePath
import System.Process (readProcess, callProcess)
import System.Info (os)
import Data.List (sort, isSuffixOf)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (finally)

data Tab = EntityTab | WorldTab deriving Eq
data Modal = ProjectPicker FilePath [FilePath] | NewProject String | Unsaved FilePath | AddEntity | SavePrefab Entity FilePath deriving Eq
data Editor = Editor
  { document :: Project, file :: FilePath, live :: Maybe (Runtime,Project)
  , selected :: Maybe Entity, paused :: Bool, gameView :: Bool, dirty :: Bool
  , logs :: [String], hud :: String, accumulator :: Float, tab :: Tab, grid :: Bool
  , hierarchyScroll :: Float, inspectorScroll :: Float, assetScroll :: Float
  , modal :: Maybe Modal, undo :: [Project], redo :: [Project], assets :: [FilePath]
  , toolMode :: Gizmo.Mode, dragUndo :: Maybe Project, pendingPrefab :: Maybe FilePath
  , engineAssetsPath :: FilePath
  }

viewProject :: Editor -> Project
viewProject s = maybe (document s) snd (live s)
isPlaying :: Editor -> Bool
isPlaying = isJust . live
addLog :: String -> Editor -> Editor
addLog message s = s {logs=take 100 (filter (not . null) (reverse (lines message))++logs s)}
withLogs :: [String] -> Editor -> Editor
withLogs ms s = foldl (flip addLog) s ms
change :: Project -> Editor -> Editor
change p s | p==document s = s
           | otherwise = s {document=p,dirty=True,undo=take 60 (document s:undo s),redo=[]}
editWorld :: (World -> World) -> Editor -> Editor
editWorld f s = change ((document s) {world=f (world (document s))}) s

runEditor :: FilePath -> FilePath -> Project -> Maybe Int -> Maybe FilePath -> IO ()
runEditor engineAssets path p frameLimit capture = do
  a<-listAssets (takeDirectory path)
  let initial=Editor p path Nothing (fst <$> IM.lookupMin (names (world p))) False False False
        ["Ready. Select an entity; F5 plays the project.","Unholy / Haskell renderer + Bullet + Lua"] "" 0 EntityTab True 0 0 0 Nothing [] [] (sort a) Gizmo.Move Nothing Nothing engineAssets
  loop 0 initial `finally` N.close
  where
    loop frame s = do
      quit<-N.shouldClose
      if quit || maybe False (frame>=) frameLimit then do
        forM_ capture N.screenshot
        forM_ (live s) (stopRuntime . fst)
        when (dirty s) $ do
          -- Recover unsaved work after a window close without overwriting the project.
          result<-saveProject (file s++".recovery") (document s)
          print result
      else do
        dt<-min 0.1 <$> N.frameTime
        input<-N.inputs
        stepped<-if modal s/=Nothing then pure s else advance dt input s
        (w,h)<-N.dimensions
        N.begin
        N.uiEnabled (modal s==Nothing)
        N.setGizmoMode (if isPlaying stepped || modal stepped/=Nothing || isJust (pendingPrefab stepped) then Nothing else Just (toolMode stepped))
        picked<-N.drawScene (takeDirectory (file s)) (viewProject stepped) (230,70,w-552,h-282)
          (not (isPlaying stepped && gameView stepped)) (grid stepped) (selected stepped)
        edit<-N.takeGizmoEdit
        let sceneState=applyGizmoEdit edit stepped {selected=picked}
        placed<-placePrefab (230,115,w-552,h-362) sceneState
        chrome<-drawChrome w h placed
        N.uiEnabled True
        next<-case modal chrome of Nothing->shortcuts chrome;Just _->drawModal w h chrome
        N.end
        err<-N.nativeError
        N.clearError
        loop (frame+1) (if null err then next else addLog err next)

advance :: Float -> [Float] -> Editor -> IO Editor
advance dt input s = case live s of
  Nothing->pure s
  Just _ | paused s->pure s
  Just (r,p)->do
    (runtime,next,msg,status,remaining)<-steps 0 (min 0.1 (accumulator s+dt)) r p [] (hud s)
    pure (withLogs msg s {live=Just (runtime,next),hud=status,accumulator=remaining})
  where
    fixed=1/120
    steps :: Int -> Float -> Runtime -> Project -> [String] -> String -> IO (Runtime,Project,[String],String,Float)
    steps count remaining r p msg status
      | remaining<fixed || count>=12 = pure (r,p,msg,status,remaining)
      | otherwise=do
          (r',p',messages,hud')<-stepRuntime fixed input r p
          steps (count+1) (remaining-fixed) r' p' (msg++messages) (if null hud' then status else hud')

startPlay :: Editor -> IO Editor
startPlay s | isPlaying s = stopPlay s
            | otherwise = do
  N.clearFocus
  missing<-checkAssets (takeDirectory (file s)) (document s)
  if not (null missing) then pure (withLogs missing s) else do
    (runtime,p,messages)<-startRuntime (takeDirectory (file s)) (document s)
    pure (withLogs messages (addLog "Play started. Stop restores the editable scene." s {live=Just (runtime,p),paused=False,gameView=True,hud="",accumulator=0,dragUndo=Nothing,pendingPrefab=Nothing}))
stopPlay :: Editor -> IO Editor
stopPlay s = do
  forM_ (live s) (stopRuntime . fst)
  N.clearFocus
  N.unloadAssets
  pure (addLog "Play stopped." s {live=Nothing,paused=False,gameView=False,hud="",accumulator=0})
save :: Editor -> IO Editor
save s = do
  result<-saveProject (file s) (document s)
  pure $ case result of Left e->addLog e s;Right ()->addLog "Project saved." s {dirty=False}
build :: FilePath -> Editor -> IO Editor
build engine s = do
  result<-buildProject (takeDirectory (file s)) engine (document s)
  pure $ either (`addLog` s) (\out->addLog ("Built: "++out++"/Play.command") s) result

drawChrome :: Float -> Float -> Editor -> IO Editor
drawChrome w h s = do
  N.rect 0 0 w 62 1
  N.text 18 18 20 "Unholy" 2
  N.text 99 26 10 "0.1" 1
  new<-N.button 174 14 61 34 "New" 0
  load<-N.button 242 14 77 34 "Load" 0
  saveClicked<-N.button 326 14 68 34 "Save" 0
  buildClicked<-N.button 401 14 70 34 "Build" 0
  play<-N.button 499 14 101 34 (if isPlaying s then "Stop  F5" else "Play  F5") (if isPlaying s then 2 else 1)
  pause<-if isPlaying s then N.button 608 14 80 34 (if paused s then "Resume" else "Pause") 0 else pure False
  step<-if isPlaying s && paused s then N.button 696 14 59 34 "Step" 0 else pure False
  N.text (w-320) 22 16 (take 28 (projectName (document s))++if dirty s then " *" else "") 0
  picker<-if load then Just <$> projectPicker (engineAssetsPath s) (file s) else pure Nothing
  let next=s {modal=if new then Just (NewProject "Untitled") else if isJust picker then picker else modal s}
  afterSave<-if saveClicked then save next else pure next
  afterBuild<-if buildClicked then do
    build (engineAssetsPath s) afterSave
    else pure afterSave
  afterPlay<-if play then startPlay afterBuild else pure afterBuild
  afterStep<-if step then case live afterPlay of
    Nothing->pure afterPlay
    Just (r,p)->do
      input<-N.inputs
      (r',p',msg,status)<-stepRuntime (1/120) input r p
      pure (withLogs msg afterPlay {live=Just (r',p'),hud=status})
    else pure afterPlay
  let state=afterStep {paused=if pause then not (paused afterStep) else paused afterStep}
  when pause (N.pauseAudio (paused state))
  N.rect 0 70 222 (h-96) 1
  N.text 16 88 13 "SCENE / ENTITIES" 1
  add<-N.button 146 80 62 29 "+ Add" 0
  sceneTab<-N.button (w-310) 80 140 30 "Entity" (if tab state==EntityTab then 3 else 0)
  worldTab<-N.button (w-162) 80 145 30 "World / Camera" (if tab state==WorldTab then 3 else 0)
  N.rect (w-322) 118 322 (h-146) 1
  let tabs=state {tab=if sceneTab then EntityTab else if worldTab then WorldTab else tab state,modal=if add && not (isPlaying state) then Just AddEntity else modal state}
  left<-drawHierarchy h tabs
  right<-drawInspector w h left
  bottom<-drawAssets w h right
  N.rect 230 (h-204) (w-560) 1 2
  N.text 245 86 12 (if isPlaying bottom then if paused bottom then "PAUSED" else "PLAYING / 120 Hz" else "SCENE VIEW") 2
  mode<-if isPlaying bottom then pure (toolMode bottom) else foldM (\current (i,label,tool)->do
    clicked<-N.button (345+fromIntegral (i::Int)*83) 79 77 28 label (if current==tool then 3 else 0)
    pure (if clicked then tool else current)) (toolMode bottom) [(0,"Move W",Gizmo.Move),(1,"Rotate E",Gizmo.Rotate),(2,"Scale R",Gizmo.Scale)]
  gameToggle<-N.button (w-464) 79 129 28 (if gameView bottom then "Game camera" else "Editor camera") 0
  N.text 245 (h-236) 12 (if isPlaying bottom then "WASD roll | Space jump | E crate | R restart" else maybe "RMB orbit | MMB pan | W/E/R tools | Shift snap | F focus" (\path->"PLACE "++takeFileName path++" / click scene / Esc cancels") (pendingPrefab bottom)) 1
  when (isPlaying bottom && not (null (hud bottom))) $ do
    N.rect 248 125 (min 600 (w-600)) 98 0
    N.text 263 140 19 (hud bottom) 0
  N.rect 0 (h-25) w 25 0
  N.text 15 (h-19) 11 (take 95 (file bottom)) 1
  N.text (w-286) (h-19) 11 (show (IM.size (names (world (viewProject bottom))))++" entities  /  "++show (IM.size (bodies (world (viewProject bottom))))++" bodies") 1
  pure bottom {toolMode=mode,gameView=if gameToggle then not (gameView bottom) else gameView bottom}

drawHierarchy :: Float -> Editor -> IO Editor
drawHierarchy h s = do
  wheel<-N.wheel 0 118 222 (h-330)
  let w=world (viewProject s)
      entries=[(e,IM.findWithDefault "" e (names w)) | root<-IM.keys (IM.difference (names w) (parents w)),e<-descendants root w]
      depth e=maybe 0 (\p->1+depth (parentEntity p)) (IM.lookup e (parents w))
      maxScroll=max 0 (fromIntegral (length entries)*35-(h-340))
      offset=max 0 (min maxScroll (hierarchyScroll s-wheel*35))
  N.clip 7 118 209 (h-336)
  picked<-foldM (\sel (i,(e,n)) -> do
    let y=122+fromIntegral (i::Int)*35-offset
    click<-N.button 12 y 196 30 (take 21 (replicate (min 6 (depth e)*2) ' '++n)) (if Just e==sel then 3 else 0)
    pure (if click then Just e else sel)) (selected s) (zip [0..] entries)
  N.unclip
  N.text 16 (h-209) 12 "PROJECT ASSETS" 1
  pure s {selected=picked,hierarchyScroll=offset,inspectorScroll=if picked/=selected s then 0 else inspectorScroll s}

drawInspector :: Float -> Float -> Editor -> IO Editor
drawInspector w h s = do
  wheel<-N.wheel (w-322) 118 322 (h-146)
  let maxScroll=if tab s==WorldTab then 600 else 1350
      scroll=max 0 (min maxScroll (inspectorScroll s-wheel*40))
      x=w-305; y=132-scroll
  N.clip (w-322) 120 322 (h-149)
  result<-if isPlaying s then do
    N.text x y 16 "Runtime inspection" 2
    N.text x (y+30) 14 "Stop play to edit the saved scene." 1
    case selected s >>= (\e->IM.lookup e (transforms (world (viewProject s)))) of
      Nothing->pure ()
      Just t->do
        N.text x (y+70) 14 "POSITION" 1
        N.text x (y+96) 16 (prettyV (position t)) 0
        N.text x (y+130) 14 "ROTATION" 1
        N.text x (y+156) 16 (prettyV (rotation t)) 0
    pure s
    else if tab s==WorldTab then worldInspector x y s else entityInspector x y s
  N.unclip
  pure result {inspectorScroll=scroll}

vectorFields :: Int -> Float -> Float -> String -> V3 -> IO V3
vectorFields ident x y title (V3 a b c) = do
  N.text x y 13 title 1
  values<-mapM (\(i,label,v)->do
    N.text (x+fromIntegral i*96) (y+24) 12 label 2
    str<-N.textbox (ident+i) (x+fromIntegral i*96+17) (y+17) 74 30 (short v)
    pure (if str==short v then v else maybe v id (readFloat str))) (zip3 [0..2] ["X","Y","Z"] [a,b,c])
  pure (fromListV values)
  where
    short v = let n=fromIntegral (round (v*1000)::Int)/1000::Float in show n

readFloat :: String -> Maybe Float
readFloat str = case reads str of [(n,"")] | finite n->Just n;_->Nothing

prettyV :: V3 -> String
prettyV = unwords . map (show . (\v->fromIntegral (round (v*100)::Int)/100::Float)) . vlist

entityInspector :: Float -> Float -> Editor -> IO Editor
entityInspector x y s = case selected s of
  Nothing->N.text x y 16 "Select an entity to inspect it." 1 >> pure s
  Just e | IM.notMember e (names w)->pure s {selected=Nothing}
  Just e->do
    N.text x y 12 ("ENTITY #"++show e++" / COMPONENTS") 2
    name<-N.textbox 10 x (y+24) 287 34 (names w IM.! e)
    dupe<-N.button x (y+68) 88 28 "Duplicate" 0
    del<-N.button (x+96) (y+68) 78 28 "Delete" 2
    prefab<-N.button (x+182) (y+68) 105 28 "Save prefab" 0
    let t=transforms w IM.! e
    pos<-vectorFields 100 x (y+115) "TRANSFORM / POSITION" (position t)
    rot<-vectorFields 110 x (y+177) "ROTATION / DEGREES" (rotation t)
    sc<-vectorFields 120 x (y+239) "SCALE" (scale t)
    let t'=Transform pos rot (fromListV (map (max 0.001) (vlist sc)))
        w0=(setTransform e t' w) {names=IM.insert e name (names w)}
    w1<-case IM.lookup e (meshes w0) of
      Nothing->do
        addMesh<-N.button x (y+309) 287 32 "Add mesh component" 0
        pure (if addMesh then w0 {meshes=IM.insert e (Mesh "cube" "") (meshes w0),materials=IM.insert e defaultMaterial (materials w0)} else w0)
      Just mesh->do
        N.text x (y+309) 13 "MESH" 2
        remove<-N.button (x+220) (y+302) 67 25 "Remove" 2
        geometry'<-N.textbox 20 x (y+335) 287 31 (geometry mesh)
        N.text x (y+376) 12 "ALBEDO TEXTURE / PROJECT PATH" 1
        tex<-N.textbox 21 x (y+398) 287 31 (texture mesh)
        N.text x (y+450) 13 "PBR MATERIAL" 2
        color<-vectorFields 130 x (y+477) "BASE COLOR / SRGB 0..1" (albedo (IM.findWithDefault defaultMaterial e (materials w0)))
        let m=IM.findWithDefault defaultMaterial e (materials w0)
        metal<-N.slider x (y+543) 287 "Metallic" (metallic m) 0 1
        rough<-N.slider x (y+591) 287 "Roughness" (roughness m) 0.04 1
        emit<-N.slider x (y+639) 287 "Emission" (emission m) 0 8
        pure $ if remove then w0 {meshes=IM.delete e (meshes w0),materials=IM.delete e (materials w0)} else w0
          {meshes=IM.insert e (Mesh geometry' tex) (meshes w0),materials=IM.insert e (Material (fromListV (map (max 0 . min 1) (vlist color))) metal rough emit) (materials w0)}
    N.text x (y+704) 13 "LUA SCRIPT" 2
    let script=IM.findWithDefault "" e (scripts w1)
    script'<-N.textbox 22 x (y+730) 287 31 script
    edit<-N.button x (y+771) 139 28 "Edit source" 0
    detach<-N.button (x+147) (y+771) 140 28 "Detach script" 0
    when (edit && not (null script')) $ openText (takeDirectory (file s) </> script')
    let w2=w1 {scripts=if detach || null script' then IM.delete e (scripts w1) else IM.insert e script' (scripts w1)}
    N.text x (y+826) 13 "BULLET RIGID BODY" 2
    w3<-case IM.lookup e (bodies w2) of
      Nothing->do
        addBody<-N.button x (y+850) 287 31 "Add box collider / static body" 0
        pure (if addBody then w2 {bodies=IM.insert e defaultBody (bodies w2)} else w2)
      Just body->do
        shape<-N.button x (y+850) 139 30 (if bodyShape body==Box then "Shape: box" else "Shape: sphere") 0
        remove<-N.button (x+147) (y+850) 140 30 "Remove body" 2
        N.text x (y+895) 13 "MASS / 0 = STATIC" 1
        massText<-N.textbox 24 (x+163) (y+887) 124 30 (show (mass body))
        friction'<-N.slider x (y+931) 287 "Friction" (friction body) 0 2
        bounce<-N.slider x (y+979) 287 "Restitution" (restitution body) 0 1
        motion<-N.button x (y+1013) 287 28 (if kinematic body then "Motion: kinematic / scripted" else "Motion: static or dynamic") 0
        sensor<-N.button x (y+1051) 287 28 (if trigger body then "Collider: trigger / no response" else "Collider: solid") 0
        damping<-N.slider x (y+1097) 287 "Linear damping" (linearDamping body) 0 1
        angular<-N.slider x (y+1145) 287 "Angular damping" (angularDamping body) 0 1
        let sensing=if sensor then not (trigger body) else trigger body
            kin=if motion then not (kinematic body) else kinematic body
        pure $ if remove then w2 {bodies=IM.delete e (bodies w2)} else w2 {bodies=IM.insert e (body {bodyShape=if shape then if bodyShape body==Box then Sphere else Box else bodyShape body,mass=if kin || sensing || IM.member e (parents w2) then 0 else max 0 (maybe (mass body) id (readFloat massText)),friction=friction',restitution=bounce,kinematic=kin,trigger=sensing,linearDamping=damping,angularDamping=angular}) (bodies w2)}
    N.text x (y+1205) 13 "POINT LIGHT" 2
    w4<-case IM.lookup e (lights w3) of
      Nothing->do
        addLight<-N.button x (y+1235) 287 30 "Add point light" 0
        pure (if addLight && IM.size (lights w3)<8 then w3 {lights=IM.insert e (Light (V3 1 0.8 0.6) 30) (lights w3)} else w3)
      Just l->do
        color<-vectorFields 140 x (y+1235) "LIGHT COLOR" (lightColor l)
        power<-N.slider x (y+1298) 287 "Intensity" (intensity l) 0 200
        remove<-N.button x (y+1346) 287 28 "Remove light" 2
        pure w3 {lights=if remove then IM.delete e (lights w3) else IM.insert e (Light color power) (lights w3)}
    N.text x (y+1410) 13 "PARENT / ENTITY ID" 2
    parentText<-N.textbox 25 x (y+1435) 116 30 (maybe "" (show . parentEntity) (IM.lookup e (parents w4)))
    attach<-N.button (x+124) (y+1435) 79 30 "Set" 0
    detachParent<-N.button (x+211) (y+1435) 76 30 "Clear" 0
    let parentChange=case reads parentText of
          [(id',"")] | IM.member id' (names w4) -> Right w4 {parents=IM.insert e (Parent id' (relative (transforms w4 IM.! id') (transforms w4 IM.! e))) (parents w4)}
          _ -> Left "Enter a live parent entity ID"
        candidate=if detachParent then Right w4 {parents=IM.delete e (parents w4)} else if attach then parentChange else Right w4
        checked=candidate >>= \value->validateWorld value >> Right value
        edited=either (\err->addLog err s) (\value->change ((document s) {world=value}) s) checked
    if del then pure (editWorld (destroy e) edited) else if dupe then do
      let (n,duplicated)=duplicate e w4
      pure (change ((document s) {world=duplicated}) s {selected=Just n})
      else pure edited {modal=if prefab then Just (SavePrefab e ("assets/prefabs/"++slug name++".prefab.json")) else modal edited}
  where w=world (document s)

worldInspector :: Float -> Float -> Editor -> IO Editor
worldInspector x y s = do
  N.text x y 13 "PROJECT" 2
  name<-N.textbox 30 x (y+26) 287 33 (projectName p)
  N.text x (y+82) 13 "LIGHTING / GGX + ACES" 2
  sun<-vectorFields 200 x (y+110) "SUN DIRECTION" (sunDirection env)
  color<-vectorFields 210 x (y+174) "SUN COLOR" (sunColor env)
  power<-N.slider x (y+241) 287 "Sun intensity" (sunPower env) 0 10
  amb<-N.slider x (y+289) 287 "Ambient" (ambient env) 0 1
  exp'<-N.slider x (y+337) 287 "Exposure" (exposure env) 0.1 4
  N.text x (y+408) 13 "GAME CAMERA" 2
  pos<-vectorFields 220 x (y+435) "EYE" (eye cam)
  tgt<-vectorFields 230 x (y+500) "TARGET" (target cam)
  fov'<-N.slider x (y+566) 287 "Field of view" (fov cam) 20 100
  useView<-N.button x (y+624) 287 32 "Use current editor camera" 0
  gridToggle<-N.button x (y+668) 287 30 (if grid s then "Grid: visible" else "Grid: hidden") 0
  gravity'<-vectorFields 260 x (y+713) "PHYSICS / GRAVITY / 120 HZ" (gravity (physicsSettings p))
  sky1<-vectorFields 240 x (y+791) "SKY / TOP COLOR" (skyTop env)
  sky2<-vectorFields 250 x (y+856) "SKY / HORIZON COLOR" (skyBottom env)
  fog<-N.slider x (y+923) 287 "Atmospheric fog" (fogDensity env) 0 0.05
  shadows<-N.button x (y+974) 287 30 (if sunShadows env then "Sun shadows: enabled" else "Sun shadows: disabled") 0
  let camera'=if pos==tgt then cam else Camera pos tgt fov'
      env'=env {sunDirection=if sun==V3 0 0 0 then sunDirection env else sun,sunColor=color,sunPower=power,ambient=amb,exposure=exp',skyTop=sky1,skyBottom=sky2,fogDensity=fog,sunShadows=if shadows then not (sunShadows env) else sunShadows env}
  finalCamera<-if useView then N.getCamera else pure camera'
  pure (change (p {projectName=name,environment=env',camera=finalCamera,physicsSettings=(physicsSettings p) {gravity=gravity'}}) s {grid=if gridToggle then not (grid s) else grid s})
  where p=document s;env=environment p;cam=camera p

drawAssets :: Float -> Float -> Editor -> IO Editor
drawAssets w h s = do
  scrollDelta<-N.wheel 0 (h-184) 222 158
  let offset=max 0 (min (max 0 (fromIntegral (length (assets s))*29-143)) (assetScroll s-scrollDelta*29))
  N.clip 9 (h-184) 203 150
  result<-foldM (\state (i,path)->do
    let y=h-181+fromIntegral (i::Int)*29-offset
    clicked<-N.button 13 y 194 25 (take 23 (takeFileName path)) 0
    if clicked && not (isPlaying state) then pure (if ".prefab.json" `isSuffixOf` path then addLog "Click the scene to place this prefab; Esc cancels." state {pendingPrefab=Just path} else attachAsset path state) else pure state) s (zip [0..] (assets s))
  N.unclip
  N.rect 230 (h-194) (w-560) 166 1
  N.text 244 (h-183) 12 "CONSOLE" 1
  refresh<-N.button (w-485) (h-187) 137 25 "Refresh assets" 0
  N.clip 241 (h-150) (w-578) 118
  forM_ (zip [0..] (take 7 (logs result))) $ \(i,msg)->N.text 244 (h-146+fromIntegral (i::Int)*17) 12 (take 140 msg) (if "error" `isSuffixOf` msg then 3 else 1)
  N.unclip
  refreshed<-if refresh then sort <$> listAssets (takeDirectory (file result)) else pure (assets result)
  when refresh N.unloadAssets
  pure result {assetScroll=offset,assets=refreshed}

attachAsset :: FilePath -> Editor -> Editor
attachAsset path s = case selected s of
  Nothing->addLog "Select an entity before attaching an asset." s
  Just e->addLog ("Attached "++path) $ editWorld (\w->case takeExtension path of
    ".lua"->w {scripts=IM.insert e path (scripts w)}
    ext | ext `elem` [".obj",".gltf",".glb",".iqm"]->w {meshes=IM.insert e (Mesh path "") (meshes w),materials=IM.insertWith (\_ old->old) e defaultMaterial (materials w)}
    ext | ext `elem` [".png",".jpg",".jpeg",".tga"]->w {meshes=IM.adjust (\m->m {texture=path}) e (meshes w)}
    _->w) s

shortcuts :: Editor -> IO Editor
shortcuts s = do
  moveKey<-N.key 87 True
  rotateKey<-N.key 69 True
  scaleKey<-N.key 82 True
  escape<-N.key 256 True
  let tool=if moveKey then Gizmo.Move else if rotateKey then Gizmo.Rotate else if scaleKey then Gizmo.Scale else toolMode s
      ready=s {toolMode=if isPlaying s then toolMode s else tool,pendingPrefab=if escape then Nothing else pendingPrefab s}
  f5<-N.key 294 True
  f<-N.key 70 True
  ctrl<-N.key 341 False
  cmd<-N.key 343 False
  saveKey<-N.key 83 True
  z<-N.key 90 True
  shift<-N.key 340 False
  when f $ forM_ (selected s >>= (\e->IM.lookup e (transforms (world (viewProject s))))) (N.focus . position)
  saved<-if (ctrl||cmd)&&saveKey then save ready else pure ready
  toggled<-if f5 then startPlay saved else pure saved
  pure $ if (ctrl||cmd)&&z && not (isPlaying toggled) then
    if shift then case redo toggled of []->toggled;p:ps->toggled {document=p,redo=ps,undo=document toggled:undo toggled,dirty=True}
    else case undo toggled of []->toggled;p:ps->toggled {document=p,undo=ps,redo=document toggled:redo toggled,dirty=True}
    else toggled

projectPicker :: FilePath -> FilePath -> IO Modal
projectPicker engine current = do
  root<-makeAbsolute (takeDirectory engine </> "projects")
  exists<-doesDirectoryExist root
  paths<-if exists then do
    entries<-listDirectory root
    fmap concat $ mapM (\e->do
      let p=root </> e </> projectFile
      valid<-doesFileExist p
      pure [p | valid]) entries
    else pure []
  pure (ProjectPicker current (sort paths))

loadIntoEditor :: FilePath -> Editor -> IO Editor
loadIntoEditor path s = do
  absolute<-makeAbsolute path
  loaded<-loadProject absolute
  case loaded of
    Left e->pure (addLog e s {modal=Nothing})
    Right p->do
      stopped<-stopPlay s
      N.unloadAssets
      a<-sort <$> listAssets (takeDirectory absolute)
      missing<-checkAssets (takeDirectory absolute) p
      pure (withLogs missing (addLog ("Loaded "++projectName p) stopped {document=p,file=absolute,selected=fst <$> IM.lookupMin (names (world p)),dirty=False,modal=Nothing,undo=[],redo=[],assets=a,inspectorScroll=0,hierarchyScroll=0,assetScroll=0,pendingPrefab=Nothing,dragUndo=Nothing}))

drawModal :: Float -> Float -> Editor -> IO Editor
drawModal w h s = do
  let x=(w-620)/2;y=(h-430)/2
  N.rect (x-4) (y-4) 628 438 2
  N.rect x y 620 430 1
  case modal s of
    Nothing->pure s
    Just (SavePrefab e path)->do
      N.text (x+24) (y+22) 22 "Save prefab assembly" 0
      N.text (x+24) (y+85) 14 "Includes the selected root and all its children." 1
      N.text (x+24) (y+125) 13 "PROJECT ASSET PATH / .prefab.json" 1
      path'<-N.textbox 902 (x+24) (y+157) 572 36 path
      write<-N.button (x+24) (y+219) 270 36 "Save prefab" 1
      cancel<-N.button (x+326) (y+219) 270 36 "Cancel" 0
      if write then do
        let valid=assetPath path' >> if ".prefab.json" `isSuffixOf` path' then Right () else Left "Use a .prefab.json filename"
        result<-case valid >> makePrefab e (world (document s)) of
          Left err->pure (Left err)
          Right prefab->savePrefab (takeDirectory (file s) </> path') prefab
        case result of
          Left err->pure (addLog err s)
          Right ()->do
            a<-sort <$> listAssets (takeDirectory (file s))
            N.clearFocus
            pure (addLog ("Saved "++path') s {assets=a,modal=Nothing})
      else pure s {modal=if cancel then Nothing else Just (SavePrefab e path')}
    Just AddEntity->do
      N.text (x+24) (y+22) 22 "Add entity" 0
      chosen<-foldM (\choice (i,label)->do
        click<-N.button (x+24) (y+71+fromIntegral (i::Int)*42) 572 34 label 0
        pure (if click then Just label else choice)) Nothing (zip [0..] (builtins++["empty","point light"]))
      cancel<-N.button (x+470) (y+381) 126 31 "Cancel" 0
      case chosen of
        Nothing->pure s {modal=if cancel then Nothing else modal s}
        Just kind->do
          let w0=world (document s)
              (e,w1)=if kind `elem` builtins then spawnMesh kind kind w0 else spawn kind w0
              w2=if kind=="point light" && IM.size (lights w1)<8 then w1 {lights=IM.insert e (Light (V3 1 0.8 0.6) 30) (lights w1)} else w1
          N.clearFocus
          pure (change ((document s) {world=w2}) s {selected=Just e,modal=Nothing,inspectorScroll=0})
    Just (NewProject name)->do
      N.text (x+24) (y+22) 22 "Create a project" 0
      N.text (x+24) (y+73) 15 "Name" 1
      name'<-N.textbox 901 (x+24) (y+102) 572 36 name
      N.text (x+24) (y+159) 14 "Creates projects/<name>-<timestamp>/ with an empty scene." 1
      create<-N.button (x+24) (y+219) 270 36 "Create project" 1
      cancel<-N.button (x+326) (y+219) 270 36 "Cancel" 0
      if create then do
        stamp<-round <$> getPOSIXTime :: IO Integer
        let path=takeDirectory (engineAssetsPath s) </> "projects" </> (slug name'++"-"++show stamp) </> projectFile
        createDirectoryIfMissing True (takeDirectory path </> "assets")
        result<-saveProject path (emptyProject name')
        case result of
          Left err->pure (addLog err s)
          Right ()->if dirty s then pure s {modal=Just (Unsaved path)} else N.clearFocus >> loadIntoEditor path s
        else pure s {modal=if cancel then Nothing else Just (NewProject name')}
    Just (ProjectPicker path projects)->do
      N.text (x+24) (y+22) 22 "Load project" 0
      chosen<-foldM (\choice (i,p)->do
        click<-N.button (x+24) (y+68+fromIntegral (i::Int)*42) 572 34 (takeFileName (takeDirectory p)) 0
        pure (if click then Just p else choice)) Nothing (zip [0..] (take 4 projects))
      N.text (x+24) (y+255) 13 "PROJECT FILE / FULL PATH" 1
      path'<-N.textbox 900 (x+24) (y+281) 572 34 path
      browse<-N.button (x+24) (y+329) 150 32 "Browse..." 0
      load<-N.button (x+183) (y+329) 201 32 "Load project" 1
      cancel<-N.button (x+393) (y+329) 203 32 "Cancel" 0
      browsed<-if browse then chooseProject else pure Nothing
      let chosen'=if load then Just path' else if isJust chosen then chosen else browsed
      case chosen' of
        Just p->if dirty s then pure s {modal=Just (Unsaved p)} else N.clearFocus >> loadIntoEditor p s
        Nothing->pure s {modal=if cancel then Nothing else Just (ProjectPicker path' projects)}
    Just (Unsaved next)->do
      N.text (x+24) (y+24) 23 "Save your scene changes?" 0
      N.text (x+24) (y+92) 16 "This project has unsaved edits." 1
      yes<-N.button (x+24) (y+162) 174 36 "Save and load" 1
      discard<-N.button (x+215) (y+162) 177 36 "Discard and load" 2
      cancel<-N.button (x+409) (y+162) 187 36 "Cancel" 0
      if yes then do saved<-save s;if dirty saved then pure saved else loadIntoEditor next saved
      else if discard then loadIntoEditor next s else pure s {modal=if cancel then Nothing else modal s}

chooseProject :: IO (Maybe FilePath)
chooseProject = do
  result<-ioEither $ if os=="darwin" then readProcess "osascript" ["-e","POSIX path of (choose file with prompt \"Choose an Unholy project JSON file\")"] ""
    else readProcess "zenity" ["--file-selection","--title=Choose an Unholy project"] ""
  pure $ either (const Nothing) (Just . takeWhile (/='\n')) result
openText :: FilePath -> IO ()
openText path = do
  exists<-doesFileExist path
  when exists $ do
    _<-ioEither (if os=="darwin" then callProcess "open" ["-t",path] else callProcess "xdg-open" [path])
    pure ()

runPlayer :: FilePath -> FilePath -> Project -> Maybe Int -> Maybe FilePath -> IO ()
runPlayer _ root p limit capture = do
  (r,started,msg)<-startRuntime root p
  mapM_ putStrLn msg
  loop 0 r started 0 "" `finally` N.close
  where
    loop frame r current acc status = do
      quit<-N.shouldClose
      escape<-N.key 256 True
      if quit||escape||maybe False (frame>=) limit then do
        forM_ capture N.screenshot
        stopRuntime r
      else do
        dt<-min 0.1 <$> N.frameTime
        input<-N.inputs
        (r',p',remaining,hud')<-step (min 0.1 (acc+dt)) r current status input
        (w,h)<-N.dimensions
        N.begin
        _<-N.drawScene root p' (0,0,w,h) False False Nothing
        when (not (null hud')) $ do N.rect 24 24 650 103 0;N.text 40 41 22 hud' 0
        N.text 26 (h-35) 14 "WASD / arrows move  |  Space jump  |  R restart  |  Esc quit" 1
        N.end
        loop (frame+1) r' p' remaining hud'
    step remaining r project status input | remaining<1/120=pure (r,project,remaining,status)
                                   | otherwise=do
      (r',p',messages,hud')<-stepRuntime (1/120) input r project
      mapM_ putStrLn messages
      step (remaining-1/120) r' p' (if null hud' then status else hud') input

applyGizmoEdit :: Maybe Gizmo.Edit -> Editor -> Editor
applyGizmoEdit Nothing s=finishDrag s
applyGizmoEdit (Just edit) s =
  let original=if Gizmo.editStarted edit then Just (document s) else dragUndo s
      next=(document s) {world=setTransform (Gizmo.editEntity edit) (Gizmo.editTransform edit) (world (document s))}
      changed=s {document=next,dirty=dirty s || next/=document s,dragUndo=original}
  in if Gizmo.editEnded edit then finishDrag changed else changed
finishDrag :: Editor -> Editor
finishDrag s=case dragUndo s of
  Just original | original/=document s -> s {undo=take 60 (original:undo s),redo=[],dragUndo=Nothing}
  _ -> s {dragUndo=Nothing}
placePrefab :: (Float,Float,Float,Float) -> Editor -> IO Editor
placePrefab rect s=case pendingPrefab s of
  Just path | not (isPlaying s) && modal s==Nothing -> do
    mouse<-N.getMouse
    if not (C.mouseEnabled mouse && C.leftPressed mouse && C.inside rect mouse) then pure s else do
      cam<-N.getCamera
      loaded<-loadPrefab (takeDirectory (file s)) path
      case loaded of
        Left err->pure (addLog err s {pendingPrefab=Nothing})
        Right prefab->do
          let C.Ray origin direction=C.screenRay rect cam mouse
              L.V3 _ oy _=origin;L.V3 _ dy _=direction
              point=if abs dy>0.001 && (-oy/dy)>0 then origin+(-oy/dy) L.*^direction else C.toLinear (target cam)
              rootPose=transforms (prefabWorld prefab) IM.! prefabRoot prefab
              V3 _ sy _=scale rootPose
              placement=identity {position=C.fromLinear (point+L.V3 0 (sy/2) 0)}
          case instantiate prefab placement (world (document s)) of
            Left err->pure (addLog err s {pendingPrefab=Nothing})
            Right (e,_,w)->pure (addLog ("Placed "++path) (change ((document s) {world=w}) s {selected=Just e,pendingPrefab=Nothing,inspectorScroll=0}))
  _ -> pure s
