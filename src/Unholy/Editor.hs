module Unholy.Editor (runEditor, runPlayer) where

import Unholy.Types
import Unholy.Transform (setTransform,relative)
import Unholy.Prefab
import qualified Unholy.Gizmo as Gizmo
import qualified Unholy.Renderer.Camera as C
import qualified Linear as L
import Unholy.Project
import qualified Unholy.Native as N
import Unholy.Game (SomeGame,validateGame,checkGameAssets,gameAssetReferences,gameComponents,gameIdentifier)
import Unholy.Authoring
import Unholy.Exception (trySync)
import Unholy.Script
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad (foldM, forM_, when)
import System.Directory
import System.FilePath
import System.Process (readProcess, callProcess)
import System.Info (os)
import Data.IORef
import Data.List (sort, isSuffixOf)
import Data.Maybe (isJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Control.Exception (finally, evaluate, onException)
import Control.DeepSeq (force)

data Tab = EntityTab | WorldTab | GameTab deriving Eq
data Modal = ProjectPicker FilePath [FilePath] | NewProject String | Unsaved FilePath | AddEntity | SavePrefab Entity FilePath deriving Eq
data Editor = Editor
  { document :: Project, file :: FilePath, live :: Maybe (Runtime,Project)
  , selected :: Maybe Entity, paused :: Bool, gameView :: Bool, dirty :: Bool
  , logs :: [String], hud :: String, accumulator :: Float, tab :: Tab, grid :: Bool
  , hierarchyScroll :: Float, inspectorScroll :: Float, assetScroll :: Float
  , modal :: Maybe Modal, undo :: [Project], redo :: [Project], assets :: [FilePath]
  , toolMode :: Gizmo.Mode, dragUndo :: Maybe Project, pendingPrefab :: Maybe FilePath
  , gameRegistry :: [SomeGame]
  , engineAssetsPath :: FilePath
  , fieldEdit :: Maybe (Int,Project)
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

runEditor :: N.Window -> [SomeGame] -> FilePath -> FilePath -> Project -> Maybe Int -> Maybe FilePath -> IO ()
runEditor window registry engineAssets path p frameLimit capture = do
  a<-listAssets (takeDirectory path)
  diagnostic<-trySync (evaluate (force (either (:[]) (const []) (validateGame registry p))))
  let initial=Editor p path Nothing (fst <$> IM.lookupMin (names (world p))) False False False
        (["Ready. Select an entity; F5 plays the project.","Unholy / Haskell renderer + Bullet + Lua"]++either (\err->[show err]) id diagnostic) "" 0 EntityTab True 0 0 0 Nothing [] [] (sort a) Gizmo.Move Nothing Nothing registry engineAssets Nothing
  active<-newIORef initial
  loop active 0 initial `finally` (do
    latest<-readIORef active
    -- A recovery or cleanup failure must not replace the original exception.
    cleanup<-trySync (forM_ (live latest) (stopRuntime . fst))
    either (putStrLn . show) pure cleanup
    when (dirty latest) $ do
      recovery<-trySync (saveProject (file latest++".recovery") (document latest))
      print recovery)
  where
    loop active frame s = do
      writeIORef active s
      quit<-N.shouldClose window
      if quit || maybe False (frame>=) frameLimit then do
        forM_ capture (N.screenshot window)
      else do
        dt<-min 0.1 <$> N.frameTime window
        input<-N.inputs window
        stepped<-if modal s/=Nothing then pure s else protectPlay window s (advance dt input s)
        writeIORef active stepped
        (w,h)<-N.dimensions window
        N.begin window
        N.uiEnabled window (modal s==Nothing)
        N.setGizmoMode window (if isPlaying stepped || modal stepped/=Nothing || isJust (pendingPrefab stepped) then Nothing else Just (toolMode stepped))
        picked<-N.drawScene window (takeDirectory (file s)) (viewProject stepped) (230,70,w-552,h-282)
          (not (isPlaying stepped && gameView stepped)) (grid stepped) (selected stepped)
        edit<-N.takeGizmoEdit window
        let sceneState=applyGizmoEdit edit stepped {selected=picked}
        writeIORef active sceneState
        placed<-placePrefab window (230,115,w-552,h-362) sceneState
        writeIORef active placed
        chrome<-drawChrome window w h placed
        writeIORef active chrome
        N.uiEnabled window True
        next<-case modal chrome of Nothing->shortcuts window chrome;Just _->drawModal window w h chrome
        writeIORef active next
        N.end window
        writeIORef active next
        err<-N.nativeError window
        N.clearError window
        loop active (frame+1) (if null err then next else addLog err next)

protectPlay :: N.Window -> Editor -> IO Editor -> IO Editor
protectPlay window original action = do
  result<-trySync $ do
    next<-action
    -- HUD/log strings from trusted Haskell code may themselves contain bottoms.
    _<-evaluate (force (hud next,logs next))
    pure next
  case result of
    Right next->pure next
    Left err->do
      stopped<-stopPlay window original
      pure (addLog ("Runtime failure: "++show err++"\nEditable scene preserved. Fix the game and start Play again.") stopped)

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

startPlay :: N.Window -> Editor -> IO Editor
startPlay window s | isPlaying s = stopPlay window s
            | otherwise = protectPlay window s $ do
  N.clearFocus window
  sceneAssets<-checkAssets (takeDirectory (file s)) (document s)
  gameAssets<-checkGameAssets (gameRegistry s) (takeDirectory (file s)) (document s)
  let missing=sceneAssets++gameAssets
  let errors=missing++either (:[]) (const []) (validateGame (gameRegistry s) (document s))
  if not (null errors) then pure (withLogs errors s) else do
    result<-trySync $ do
      started@(runtime,_,messages)<-startRuntimeWith (gameRegistry s) (N.audio window) (takeDirectory (file s)) (document s)
      _<-evaluate (force messages) `onException` stopRuntime runtime
      pure started
    case result of
      Left err->pure (addLog (show err) s)
      Right (runtime,p,messages)->pure (withLogs messages (addLog "Play started. Stop restores the editable scene." s {live=Just (runtime,p),paused=False,gameView=True,hud="",accumulator=0,dragUndo=Nothing,pendingPrefab=Nothing}))
stopPlay :: N.Window -> Editor -> IO Editor
stopPlay window s = do
  closed<-trySync (forM_ (live s) (stopRuntime . fst))
  N.clearFocus window
  N.unloadAssets window
  pure (either (addLog . show) (const id) closed (addLog "Play stopped." s {live=Nothing,paused=False,gameView=False,hud="",accumulator=0}))
save :: Editor -> IO Editor
save s = do
  result<-saveProject (file s) (document s)
  pure $ case result of Left e->addLog e s;Right ()->addLog "Project saved." s {dirty=False}
build :: FilePath -> Editor -> IO Editor
build engine s = do
  attempted<-trySync $ do
    missing<-checkGameAssets (gameRegistry s) (takeDirectory (file s)) (document s)
    let refs=validateGame (gameRegistry s) (document s) >> gameAssetReferences (gameRegistry s) (document s)
    result<-if null missing then case refs of
      Left err->pure (Left err)
      Right paths->buildProjectWithAssets paths (takeDirectory (file s)) engine (document s)
      else pure (Left (unlines missing))
    evaluate (force result)
  let result=either (Left . show) id attempted
  pure $ either (`addLog` s) (\out->addLog ("Built: "++out++"/Play.command") s) result

drawChrome :: N.Window -> Float -> Float -> Editor -> IO Editor
drawChrome window w h s = do
  N.rect window 0 0 w 62 1
  N.text window 18 18 20 "Unholy" 2
  N.text window 99 26 10 "0.1" 1
  new<-N.button window 174 14 61 34 "New" 0
  load<-N.button window 242 14 77 34 "Load" 0
  saveClicked<-N.button window 326 14 68 34 "Save" 0
  buildClicked<-N.button window 401 14 70 34 "Build" 0
  play<-N.button window 499 14 101 34 (if isPlaying s then "Stop  F5" else "Play  F5") (if isPlaying s then 2 else 1)
  pause<-if isPlaying s then N.button window 608 14 80 34 (if paused s then "Resume" else "Pause") 0 else pure False
  step<-if isPlaying s && paused s then N.button window 696 14 59 34 "Step" 0 else pure False
  N.text window (w-320) 22 16 (take 28 (projectName (document s))++if dirty s then " *" else "") 0
  picker<-if load then Just <$> projectPicker (engineAssetsPath s) (file s) else pure Nothing
  let next=s {modal=if new then Just (NewProject "Untitled") else if isJust picker then picker else modal s}
  afterSave<-if saveClicked then save next else pure next
  afterBuild<-if buildClicked then do
    build (engineAssetsPath s) afterSave
    else pure afterSave
  afterPlay<-if play then startPlay window afterBuild else pure afterBuild
  afterStep<-if step then case live afterPlay of
    Nothing->pure afterPlay
    Just (r,p)->protectPlay window afterPlay $ do
      input<-N.inputs window
      (r',p',msg,status)<-stepRuntime (1/120) input r p
      pure (withLogs msg afterPlay {live=Just (r',p'),hud=status})
    else pure afterPlay
  let state=afterStep {paused=if pause then not (paused afterStep) else paused afterStep}
  when pause (N.pauseAudio window (paused state))
  N.rect window 0 70 222 (h-96) 1
  N.text window 16 88 13 "SCENE / ENTITIES" 1
  add<-N.button window 146 80 62 29 "+ Add" 0
  sceneTab<-N.button window (w-310) 80 92 30 "Entity" (if tab state==EntityTab then 3 else 0)
  worldTab<-N.button window (w-210) 80 96 30 "World" (if tab state==WorldTab then 3 else 0)
  gameTab<-N.button window (w-106) 80 91 30 "Game" (if tab state==GameTab then 3 else 0)
  N.rect window (w-322) 118 322 (h-146) 1
  let tabs=state {tab=if sceneTab then EntityTab else if worldTab then WorldTab else if gameTab then GameTab else tab state,modal=if add && not (isPlaying state) then Just AddEntity else modal state}
  when (tab tabs/=tab state) (N.clearFocus window)
  left<-drawHierarchy window h tabs
  right<-drawInspector window w h left
  bottom<-drawAssets window w h right
  N.rect window 230 (h-204) (w-560) 1 2
  N.text window 245 86 12 (if isPlaying bottom then if paused bottom then "PAUSED" else "PLAYING / 120 Hz" else "SCENE VIEW") 2
  mode<-if isPlaying bottom then pure (toolMode bottom) else foldM (\current (i,label,tool)->do
    clicked<-N.button window (345+fromIntegral (i::Int)*83) 79 77 28 label (if current==tool then 3 else 0)
    pure (if clicked then tool else current)) (toolMode bottom) [(0,"Move W",Gizmo.Move),(1,"Rotate E",Gizmo.Rotate),(2,"Scale R",Gizmo.Scale)]
  gameToggle<-N.button window (w-464) 79 129 28 (if gameView bottom then "Game camera" else "Editor camera") 0
  N.text window 245 (h-236) 12 (if isPlaying bottom then "WASD move | Space jump | E action | R restart" else maybe "RMB orbit | MMB pan | W/E/R tools | Shift snap | F focus" (\path->"PLACE "++takeFileName path++" / click scene / Esc cancels") (pendingPrefab bottom)) 1
  when (isPlaying bottom && not (null (hud bottom))) $ do
    N.rect window 248 125 (min 600 (w-600)) 98 0
    N.text window 263 140 19 (hud bottom) 0
  N.rect window 0 (h-25) w 25 0
  N.text window 15 (h-19) 11 (take 95 (file bottom)) 1
  N.text window (w-286) (h-19) 11 (show (IM.size (names (world (viewProject bottom))))++" entities  /  "++show (IM.size (bodies (world (viewProject bottom))))++" bodies") 1
  pure bottom {toolMode=mode,gameView=if gameToggle then not (gameView bottom) else gameView bottom}

drawHierarchy :: N.Window -> Float -> Editor -> IO Editor
drawHierarchy window h s = do
  wheel<-N.wheel window 0 118 222 (h-330)
  let w=world (viewProject s)
      entries=[(e,IM.findWithDefault "" e (names w)) | root<-IM.keys (IM.difference (names w) (parents w)),e<-descendants root w]
      depth e=maybe 0 (\p->1+depth (parentEntity p)) (IM.lookup e (parents w))
      maxScroll=max 0 (fromIntegral (length entries)*35-(h-340))
      offset=max 0 (min maxScroll (hierarchyScroll s-wheel*35))
  N.clip window 7 118 209 (h-336)
  picked<-foldM (\sel (i,(e,n)) -> do
    let y=122+fromIntegral (i::Int)*35-offset
    click<-N.button window 12 y 196 30 (take 21 (replicate (min 6 (depth e)*2) ' '++n)) (if Just e==sel then 3 else 0)
    pure (if click then Just e else sel)) (selected s) (zip [0..] entries)
  N.unclip window
  N.text window 16 (h-209) 12 "PROJECT ASSETS" 1
  when (picked/=selected s) (N.clearFocus window)
  pure s {selected=picked,hierarchyScroll=offset,inspectorScroll=if picked/=selected s then 0 else inspectorScroll s,fieldEdit=if picked/=selected s then Nothing else fieldEdit s}

drawInspector :: N.Window -> Float -> Float -> Editor -> IO Editor
drawInspector window w h s = do
  wheel<-N.wheel window (w-322) 118 322 (h-146)
  let maxScroll=if tab s==WorldTab then 820 else 1660
      scroll=max 0 (min maxScroll (inspectorScroll s-wheel*40))
      x=w-305; y=132-scroll
  N.clip window (w-322) 120 322 (h-149)
  result<-if isPlaying s then do
    N.text window x y 16 "Runtime inspection" 2
    N.text window x (y+30) 14 "Stop play to edit the saved scene." 1
    case selected s >>= (\e->IM.lookup e (transforms (world (viewProject s)))) of
      Nothing->pure ()
      Just t->do
        N.text window x (y+70) 14 "POSITION" 1
        N.text window x (y+96) 16 (prettyV (position t)) 0
        N.text window x (y+130) 14 "ROTATION" 1
        N.text window x (y+156) 16 (prettyV (rotation t)) 0
    pure s
    else case tab s of
      WorldTab->worldInspector window x y s
      GameTab->gameInspector window x y s
      EntityTab->entityInspector window x y s
  N.unclip window
  gesture<-N.editGesture window
  let changed=document result/=document s
      continuing=case (fieldEdit s,gesture) of (Just (old,_),Just current)->old==current;_->False
      marker=case gesture of
        Nothing->Nothing
        Just ident->if continuing then fieldEdit s else if changed then Just (ident,document s) else Nothing
      history=if changed && continuing then undo s else undo result
  pure result {inspectorScroll=scroll,undo=history,fieldEdit=marker}

gameInspector :: N.Window -> Float -> Float -> Editor -> IO Editor
gameInspector window x y s=do
  N.text window x y 13 "COMPILED HASKELL GAME" 2
  let keys=Nothing:map (Just . gameIdentifier) (gameRegistry s)
      current=haskellGame (document s)
      nextKey=case dropWhile (/=current) keys of _:value:_->value;_->Nothing
  choose<-N.button window x (y+25) 287 32 (maybe "Lua / scene only" id current++"  >") 0
  let chosen=if choose then change ((document s) {haskellGame=nextKey}) s else s
  case (selected chosen,gameComponents (gameRegistry chosen) (document chosen)) of
    (_,Left err)->N.text window x (y+72) 13 err 2 >> pure chosen
    (Nothing,_)->N.text window x (y+72) 14 "Select an entity for gameplay fields." 1 >> pure chosen
    (Just e,Right definitions)->do
      (result,_)<-foldM (drawComponent e) (chosen,y+76) (zip [0..] definitions)
      let registered=[componentKey d | SomeComponent d<-definitions]
          unknown=[key | key<-M.keys (IM.findWithDefault M.empty e (authored (world (document result)))),key `notElem` registered]
      forM_ (zip [0..] unknown) $ \(i,key)->N.text window x (y+600+fromIntegral (i::Int)*24) 12 ("Preserved unavailable component: "++key) 2
      pure result
  where
    drawComponent e (state,top) (i,SomeComponent definition)=do
      let key=componentKey definition
          w=world (document state)
          existing=IM.lookup e (authored w) >>= M.lookup key
      N.text window x top 15 key 2
      clicked<-N.button window (x+204) (top-4) 83 28 (maybe "Add" (const "Remove") existing) 0
      if clicked then pure (editWorld (maybe (setComponent definition e (componentDefault definition)) (const (removeComponent key e)) existing) state,top+44)
      else case existing of
        Nothing->pure (state,top+44)
        Just fields->do
          (edited,endY)<-foldM (drawField e key i) (fields,top+34) (zip [0..] (componentFields definition))
          let next=editWorld (\currentWorld->currentWorld {authored=IM.adjust (M.insert key edited) e (authored currentWorld)}) state
          pure (next,endY+18)
    drawField _ _ group (fields,top) (i,description)=do
      let key=fieldKey description
          current=M.lookup key fields
          ident=2000+group*100+i
      N.text window x top 12 (fieldLabel description) 1
      value<-case (fieldKind description,current) of
        (NumberField lo hi,Just (FloatValue number)) | finite number && number>=lo && number<=hi->do
          FloatValue <$> N.slider window x top 287 "" number lo hi
        (TextField,Just (TextValue txt))->TextValue <$> N.textbox window ident x (top+19) 287 30 txt
        (ToggleField,Just (BoolValue flag))->do
          toggle<-N.button window x (top+19) 287 30 (if flag then "Enabled" else "Disabled") 0
          pure (BoolValue (if toggle then not flag else flag))
        (EntityField,Just (EntityValue ref))->do
          let w=world (document s); choices=Nothing:map Just (IM.keys (names w))
              next=case dropWhile (/=ref) choices of _:value:_->value;_->Nothing
              label=maybe "None" (\e->show e++" / "++IM.findWithDefault "Missing" e (names w)) ref
          pick<-N.button window x (top+19) 287 30 (label++"  >") 0
          pure (EntityValue (if pick then next else ref))
        _->N.text window x (top+22) 12 "Invalid field; remove and re-add component" 2 >> pure (maybe (TextValue "") id current)
      pure (if current==Nothing then fields else M.insert key value fields,top+61)

vectorFields :: N.Window -> Int -> Float -> Float -> String -> V3 -> IO V3
vectorFields window ident x y title (V3 a b c) = do
  N.text window x y 13 title 1
  values<-mapM (\(i,label,v)->do
    N.text window (x+fromIntegral i*96) (y+24) 12 label 2
    str<-N.textbox window (ident+i) (x+fromIntegral i*96+17) (y+17) 74 30 (short v)
    pure (if str==short v then v else maybe v id (readFloat str))) (zip3 [0..2] ["X","Y","Z"] [a,b,c])
  pure (fromListV values)
  where
    short v = let n=fromIntegral (round (v*1000)::Int)/1000::Float in show n

readFloat :: String -> Maybe Float
readFloat str = case reads str of [(n,"")] | finite n->Just n;_->Nothing

prettyV :: V3 -> String
prettyV = unwords . map (show . (\v->fromIntegral (round (v*100)::Int)/100::Float)) . vlist

entityInspector :: N.Window -> Float -> Float -> Editor -> IO Editor
entityInspector window x y s = case selected s of
  Nothing->N.text window x y 16 "Select an entity to inspect it." 1 >> pure s
  Just e | IM.notMember e (names w)->pure s {selected=Nothing}
  Just e->do
    N.text window x y 12 ("ENTITY #"++show e++" / COMPONENTS") 2
    name<-N.textbox window 10 x (y+24) 287 34 (names w IM.! e)
    dupe<-N.button window x (y+68) 88 28 "Duplicate" 0
    del<-N.button window (x+96) (y+68) 78 28 "Delete" 2
    prefab<-N.button window (x+182) (y+68) 105 28 "Save prefab" 0
    let t=transforms w IM.! e
    pos<-vectorFields window 100 x (y+115) "TRANSFORM / POSITION" (position t)
    rot<-vectorFields window 110 x (y+177) "ROTATION / DEGREES" (rotation t)
    sc<-vectorFields window 120 x (y+239) "SCALE" (scale t)
    let t'=Transform pos rot (fromListV (map (max 0.001) (vlist sc)))
        w0=(setTransform e t' w) {names=IM.insert e name (names w)}
    w1<-case IM.lookup e (meshes w0) of
      Nothing->do
        addMesh<-N.button window x (y+309) 287 32 "Add mesh component" 0
        pure (if addMesh then w0 {meshes=IM.insert e (Mesh "cube" "" False) (meshes w0),materials=IM.insert e defaultMaterial (materials w0)} else w0)
      Just mesh->do
        N.text window x (y+309) 13 "MESH" 2
        remove<-N.button window (x+220) (y+302) 67 25 "Remove" 2
        geometry'<-N.textbox window 20 x (y+335) 287 31 (geometry mesh)
        N.text window x (y+376) 12 "ALBEDO TEXTURE / PROJECT PATH" 1
        tex<-N.textbox window 21 x (y+398) 287 31 (texture mesh)
        N.text window x (y+450) 13 "PBR MATERIAL" 2
        policy<-N.button window (x+127) (y+442) 160 28 (if useImportedMaterials mesh then "Imported slots" else "Entity override") 0
        color<-vectorFields window 130 x (y+477) "BASE COLOR / SRGB 0..1" (albedo (IM.findWithDefault defaultMaterial e (materials w0)))
        let m=IM.findWithDefault defaultMaterial e (materials w0)
        metal<-N.slider window x (y+543) 287 "Metallic" (metallic m) 0 1
        rough<-N.slider window x (y+591) 287 "Roughness" (roughness m) 0.04 1
        emit<-N.slider window x (y+639) 287 "Emission" (emission m) 0 8
        pure $ if remove then w0 {meshes=IM.delete e (meshes w0),animators=IM.delete e (animators w0),materials=IM.delete e (materials w0)} else w0
          {meshes=IM.insert e (Mesh geometry' tex (if policy then not (useImportedMaterials mesh) else useImportedMaterials mesh)) (meshes w0),materials=IM.insert e (Material (fromListV (map (max 0 . min 1) (vlist color))) metal rough emit) (materials w0)}
    N.text window x (y+704) 13 "LUA SCRIPT" 2
    let script=IM.findWithDefault "" e (scripts w1)
    script'<-N.textbox window 22 x (y+730) 287 31 script
    edit<-N.button window x (y+771) 139 28 "Edit source" 0
    detach<-N.button window (x+147) (y+771) 140 28 "Detach script" 0
    when (edit && not (null script')) $ openText (takeDirectory (file s) </> script')
    let w2=w1 {scripts=if detach || null script' then IM.delete e (scripts w1) else IM.insert e script' (scripts w1)}
    N.text window x (y+826) 13 "BULLET RIGID BODY" 2
    w3<-case IM.lookup e (bodies w2) of
      Nothing->do
        addBody<-N.button window x (y+850) 287 31 "Add box collider / static body" 0
        pure (if addBody then w2 {bodies=IM.insert e defaultBody (bodies w2)} else w2)
      Just body->do
        shape<-N.button window x (y+850) 139 30 ("Shape: "++shapeName (bodyShape body)) 0
        remove<-N.button window (x+147) (y+850) 140 30 "Remove body" 2
        N.text window x (y+895) 13 "MASS / 0 = STATIC" 1
        massText<-N.textbox window 24 (x+163) (y+887) 124 30 (show (mass body))
        friction'<-N.slider window x (y+931) 287 "Friction" (friction body) 0 2
        bounce<-N.slider window x (y+979) 287 "Restitution" (restitution body) 0 1
        motion<-N.button window x (y+1013) 287 28 (if kinematic body then "Motion: kinematic / scripted" else "Motion: static or dynamic") 0
        sensor<-N.button window x (y+1051) 287 28 (if trigger body then "Collider: trigger / no response" else "Collider: solid") 0
        damping<-N.slider window x (y+1097) 287 "Linear damping" (linearDamping body) 0 1
        angular<-N.slider window x (y+1145) 287 "Angular damping" (angularDamping body) 0 1
        upright<-N.button window x (y+1180) 287 23 (if lockRotation body then "Rotation: locked / upright" else "Rotation: free") 0
        let choices=drop 1 (dropWhile (/=bodyShape body) (cycle [minBound..maxBound]))
            allowed candidate=let trial=body {bodyShape=candidate,mass=if candidate==TriangleMesh then 0 else mass body,kinematic=if candidate==TriangleMesh then False else kinematic body}
                              in validateWorld w2 {bodies=IM.insert e trial (bodies w2)}==Right ()
            nextShape=if shape then case filter allowed (take 5 choices) of value:_->value;_->bodyShape body else bodyShape body
            sensing=if sensor then not (trigger body) else trigger body
            kin=nextShape/=TriangleMesh && (if motion then not (kinematic body) else kinematic body)
        pure $ if remove then w2 {bodies=IM.delete e (bodies w2)} else w2 {bodies=IM.insert e (body {bodyShape=nextShape,lockRotation=if upright then not (lockRotation body) else lockRotation body,mass=if nextShape==TriangleMesh || kin || sensing || IM.member e (parents w2) then 0 else max 0 (maybe (mass body) id (readFloat massText)),friction=friction',restitution=bounce,kinematic=kin,trigger=sensing,linearDamping=damping,angularDamping=angular}) (bodies w2)}
    N.text window x (y+1205) 13 "POINT LIGHT" 2
    w4<-case IM.lookup e (lights w3) of
      Nothing->do
        addLight<-N.button window x (y+1235) 287 30 "Add point light" 0
        pure (if addLight && IM.size (lights w3)<8 then w3 {lights=IM.insert e (Light (V3 1 0.8 0.6) 30) (lights w3)} else w3)
      Just l->do
        color<-vectorFields window 140 x (y+1235) "LIGHT COLOR" (lightColor l)
        power<-N.slider window x (y+1298) 287 "Intensity" (intensity l) 0 200
        remove<-N.button window x (y+1346) 287 28 "Remove light" 2
        pure w3 {lights=if remove then IM.delete e (lights w3) else IM.insert e (Light color power) (lights w3)}
    N.text window x (y+1510) 13 "SKELETAL ANIMATOR" 2
    w5<-case IM.lookup e (animators w4) of
      Nothing->do
        add<-N.button window x (y+1535) 287 30 "Add Animator / glTF skin" 0
        pure (if add && IM.member e (meshes w4) then w4 {animators=IM.insert e defaultAnimator (animators w4)} else w4)
      Just a->do
        clip<-N.textbox window 26 x (y+1535) 287 30 (animationClip a)
        phase<-N.textbox window 27 x (y+1575) 139 30 (show (animationTime a))
        rate<-N.textbox window 28 (x+147) (y+1575) 140 30 (show (animationRate a))
        N.text window x (y+1610) 12 "Pose seconds / playback rate 0..10" 1
        loop<-N.button window x (y+1635) 139 30 (if animationLoop a then "Loop" else "Clamp") 0
        play<-N.button window (x+147) (y+1635) 140 30 (if animationPlaying a then "Playing" else "Paused") 0
        remove<-N.button window x (y+1675) 287 30 "Remove Animator" 2
        let next=a {animationClip=clip,animationTime=max 0 (maybe (animationTime a) id (readFloat phase)),animationRate=max 0 (min 10 (maybe (animationRate a) id (readFloat rate))),animationLoop=if loop then not (animationLoop a) else animationLoop a,animationPlaying=if play then not (animationPlaying a) else animationPlaying a}
        pure w4 {animators=if remove then IM.delete e (animators w4) else IM.insert e next (animators w4)}
    N.text window x (y+1410) 13 "PARENT / ENTITY ID" 2
    parentText<-N.textbox window 25 x (y+1435) 116 30 (maybe "" (show . parentEntity) (IM.lookup e (parents w4)))
    attach<-N.button window (x+124) (y+1435) 79 30 "Set" 0
    detachParent<-N.button window (x+211) (y+1435) 76 30 "Clear" 0
    let parentChange=case reads parentText of
          [(id',"")] | IM.member id' (names w4) -> Right w5 {parents=IM.insert e (Parent id' (relative (transforms w4 IM.! id') (transforms w4 IM.! e))) (parents w4)}
          _ -> Left "Enter a live parent entity ID"
        candidate=if detachParent then Right w5 {parents=IM.delete e (parents w4)} else if attach then parentChange else Right w5
        checked=candidate >>= \value->validateWorld value >> Right value
        edited=either (\err->addLog err s) (\value->change ((document s) {world=value}) s) checked
    if del then pure (editWorld (destroy e) edited) else if dupe then do
      let (n,duplicated)=duplicate e w5
      pure (change ((document s) {world=duplicated}) s {selected=Just n})
      else pure edited {modal=if prefab then Just (SavePrefab e ("assets/prefabs/"++slug name++".prefab.json")) else modal edited}
  where w=world (document s)

worldInspector :: N.Window -> Float -> Float -> Editor -> IO Editor
worldInspector window x y s = do
  N.text window x y 13 "PROJECT" 2
  name<-N.textbox window 30 x (y+26) 287 33 (projectName p)
  N.text window x (y+82) 13 "LIGHTING / GGX + ACES" 2
  sun<-vectorFields window 200 x (y+110) "SUN DIRECTION" (sunDirection env)
  color<-vectorFields window 210 x (y+174) "SUN COLOR" (sunColor env)
  power<-N.slider window x (y+241) 287 "Sun intensity" (sunPower env) 0 10
  amb<-N.slider window x (y+289) 287 "Ambient" (ambient env) 0 1
  exp'<-N.slider window x (y+337) 287 "Exposure" (exposure env) 0.1 4
  N.text window x (y+408) 13 "GAME CAMERA" 2
  pos<-vectorFields window 220 x (y+435) "EYE" (eye cam)
  tgt<-vectorFields window 230 x (y+500) "TARGET" (target cam)
  fov'<-N.slider window x (y+566) 287 "Field of view" (fov cam) 20 100
  useView<-N.button window x (y+624) 287 32 "Use current editor camera" 0
  gridToggle<-N.button window x (y+668) 287 30 (if grid s then "Grid: visible" else "Grid: hidden") 0
  gravity'<-vectorFields window 260 x (y+713) "PHYSICS / GRAVITY / 120 HZ" (gravity (physicsSettings p))
  sky1<-vectorFields window 240 x (y+791) "SKY / TOP COLOR" (skyTop env)
  sky2<-vectorFields window 250 x (y+856) "SKY / HORIZON COLOR" (skyBottom env)
  fog<-N.slider window x (y+923) 287 "Atmospheric fog" (fogDensity env) 0 0.05
  shadows<-N.button window x (y+974) 287 30 (if sunShadows env then "Sun shadows: enabled" else "Sun shadows: disabled") 0
  N.text window x (y+1030) 13 "HDR ENVIRONMENT / RADIANCE" 2
  hdr<-N.textbox window 270 x (y+1055) 287 33 (environmentMap env)
  hdrPower<-N.slider window x (y+1105) 287 "Environment intensity" (environmentIntensity env) 0 20
  hdrRotation<-N.slider window x (y+1153) 287 "Environment rotation" (environmentRotation env) (-180) 180
  let camera'=if pos==tgt then cam else Camera pos tgt fov'
      env'=env {sunDirection=if sun==V3 0 0 0 then sunDirection env else sun,sunColor=color,sunPower=power,ambient=amb,exposure=exp',skyTop=sky1,skyBottom=sky2,fogDensity=fog,environmentMap=hdr,environmentIntensity=hdrPower,environmentRotation=hdrRotation,sunShadows=if shadows then not (sunShadows env) else sunShadows env}
  finalCamera<-if useView then N.getCamera window else pure camera'
  pure (change (p {projectName=name,environment=env',camera=finalCamera,physicsSettings=(physicsSettings p) {gravity=gravity'}}) s {grid=if gridToggle then not (grid s) else grid s})
  where p=document s;env=environment p;cam=camera p

drawAssets :: N.Window -> Float -> Float -> Editor -> IO Editor
drawAssets window w h s = do
  scrollDelta<-N.wheel window 0 (h-184) 222 158
  let offset=max 0 (min (max 0 (fromIntegral (length (assets s))*29-143)) (assetScroll s-scrollDelta*29))
  N.clip window 9 (h-184) 203 150
  result<-foldM (\state (i,path)->do
    let y=h-181+fromIntegral (i::Int)*29-offset
    clicked<-N.button window 13 y 194 25 (take 23 (takeFileName path)) 0
    if clicked && not (isPlaying state) then pure (if ".prefab.json" `isSuffixOf` path then addLog "Click the scene to place this prefab; Esc cancels." state {pendingPrefab=Just path} else attachAsset path state) else pure state) s (zip [0..] (assets s))
  N.unclip window
  N.rect window 230 (h-194) (w-560) 166 1
  N.text window 244 (h-183) 12 "CONSOLE" 1
  refresh<-N.button window (w-485) (h-187) 137 25 "Refresh assets" 0
  N.clip window 241 (h-150) (w-578) 118
  forM_ (zip [0..] (take 7 (logs result))) $ \(i,msg)->N.text window 244 (h-146+fromIntegral (i::Int)*17) 12 (take 140 msg) (if "error" `isSuffixOf` msg then 3 else 1)
  N.unclip window
  refreshed<-if refresh then sort <$> listAssets (takeDirectory (file result)) else pure (assets result)
  when refresh (N.unloadAssets window)
  pure result {assetScroll=offset,assets=refreshed}

attachAsset :: FilePath -> Editor -> Editor
attachAsset path s | takeExtension path==".hdr" = addLog ("Environment: "++path) $ change ((document s) {environment=(environment (document s)) {environmentMap=path}}) s {tab=WorldTab}
attachAsset path s = case selected s of
  Nothing->addLog "Select an entity before attaching an asset." s
  Just e->addLog ("Attached "++path) $ editWorld (\w->case takeExtension path of
    ".lua"->w {scripts=IM.insert e path (scripts w)}
    ext | ext `elem` [".obj",".gltf",".glb",".iqm"]->w {meshes=IM.insert e (Mesh path "" (ext `elem` [".gltf",".glb"])) (meshes w),materials=IM.insertWith (\_ old->old) e defaultMaterial (materials w)}
    ext | ext `elem` [".png",".jpg",".jpeg",".tga"]->w {meshes=IM.adjust (\m->m {texture=path}) e (meshes w)}
    _->w) s

shortcuts :: N.Window -> Editor -> IO Editor
shortcuts window s = do
  moveKey<-N.key window 87 True
  rotateKey<-N.key window 69 True
  scaleKey<-N.key window 82 True
  escape<-N.key window 256 True
  let tool=if moveKey then Gizmo.Move else if rotateKey then Gizmo.Rotate else if scaleKey then Gizmo.Scale else toolMode s
      ready=s {toolMode=if isPlaying s then toolMode s else tool,pendingPrefab=if escape then Nothing else pendingPrefab s}
  f5<-N.key window 294 True
  f<-N.key window 70 True
  ctrl<-N.key window 341 False
  cmd<-N.key window 343 False
  saveKey<-N.key window 83 True
  z<-N.key window 90 True
  shift<-N.key window 340 False
  when f $ forM_ (selected s >>= (\e->IM.lookup e (transforms (world (viewProject s))))) (N.focus window . position)
  saved<-if (ctrl||cmd)&&saveKey then save ready else pure ready
  toggled<-if f5 then startPlay window saved else pure saved
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

loadIntoEditor :: N.Window -> FilePath -> Editor -> IO Editor
loadIntoEditor window path s = do
  absolute<-makeAbsolute path
  loaded<-loadProject absolute
  case loaded of
    Left e->pure (addLog e s {modal=Nothing})
    Right p->do
      stopped<-stopPlay window s
      N.unloadAssets window
      a<-sort <$> listAssets (takeDirectory absolute)
      missing<-checkAssets (takeDirectory absolute) p
      pure (withLogs missing (addLog ("Loaded "++projectName p) stopped {document=p,file=absolute,selected=fst <$> IM.lookupMin (names (world p)),dirty=False,modal=Nothing,undo=[],redo=[],assets=a,inspectorScroll=0,hierarchyScroll=0,assetScroll=0,pendingPrefab=Nothing,dragUndo=Nothing}))

drawModal :: N.Window -> Float -> Float -> Editor -> IO Editor
drawModal window w h s = do
  let x=(w-620)/2;y=(h-430)/2
  N.rect window (x-4) (y-4) 628 438 2
  N.rect window x y 620 430 1
  case modal s of
    Nothing->pure s
    Just (SavePrefab e path)->do
      N.text window (x+24) (y+22) 22 "Save prefab assembly" 0
      N.text window (x+24) (y+85) 14 "Includes the selected root and all its children." 1
      N.text window (x+24) (y+125) 13 "PROJECT ASSET PATH / .prefab.json" 1
      path'<-N.textbox window 902 (x+24) (y+157) 572 36 path
      write<-N.button window (x+24) (y+219) 270 36 "Save prefab" 1
      cancel<-N.button window (x+326) (y+219) 270 36 "Cancel" 0
      if write then do
        let valid=assetPath path' >> if ".prefab.json" `isSuffixOf` path' then Right () else Left "Use a .prefab.json filename"
        result<-case valid >> makePrefab e (world (document s)) of
          Left err->pure (Left err)
          Right prefab->savePrefab (takeDirectory (file s) </> path') prefab
        case result of
          Left err->pure (addLog err s)
          Right ()->do
            a<-sort <$> listAssets (takeDirectory (file s))
            N.clearFocus window
            pure (addLog ("Saved "++path') s {assets=a,modal=Nothing})
      else pure s {modal=if cancel then Nothing else Just (SavePrefab e path')}
    Just AddEntity->do
      N.text window (x+24) (y+22) 22 "Add entity" 0
      chosen<-foldM (\choice (i,label)->do
        click<-N.button window (x+24) (y+71+fromIntegral (i::Int)*36) 572 34 label 0
        pure (if click then Just label else choice)) Nothing (zip [0..] (builtins++["empty","point light"]))
      cancel<-N.button window (x+470) (y+381) 126 31 "Cancel" 0
      case chosen of
        Nothing->pure s {modal=if cancel then Nothing else modal s}
        Just kind->do
          let w0=world (document s)
              (e,w1)=if kind `elem` builtins then spawnMesh kind kind w0 else spawn kind w0
              w2=if kind=="point light" && IM.size (lights w1)<8 then w1 {lights=IM.insert e (Light (V3 1 0.8 0.6) 30) (lights w1)} else w1
          N.clearFocus window
          pure (change ((document s) {world=w2}) s {selected=Just e,modal=Nothing,inspectorScroll=0})
    Just (NewProject name)->do
      N.text window (x+24) (y+22) 22 "Create a project" 0
      N.text window (x+24) (y+73) 15 "Name" 1
      name'<-N.textbox window 901 (x+24) (y+102) 572 36 name
      N.text window (x+24) (y+159) 14 "Creates projects/<name>-<timestamp>/ with an empty scene." 1
      create<-N.button window (x+24) (y+219) 270 36 "Create project" 1
      cancel<-N.button window (x+326) (y+219) 270 36 "Cancel" 0
      if create then do
        stamp<-round <$> getPOSIXTime :: IO Integer
        let path=takeDirectory (engineAssetsPath s) </> "projects" </> (slug name'++"-"++show stamp) </> projectFile
        createDirectoryIfMissing True (takeDirectory path </> "assets")
        result<-saveProject path (emptyProject name')
        case result of
          Left err->pure (addLog err s)
          Right ()->if dirty s then pure s {modal=Just (Unsaved path)} else N.clearFocus window >> loadIntoEditor window path s
        else pure s {modal=if cancel then Nothing else Just (NewProject name')}
    Just (ProjectPicker path projects)->do
      N.text window (x+24) (y+22) 22 "Load project" 0
      nextPage<-N.button window (x+470) (y+22) 126 30 "More projects >" 0
      chosen<-foldM (\choice (i,p)->do
        click<-N.button window (x+24) (y+68+fromIntegral (i::Int)*42) 572 34 (takeFileName (takeDirectory p)) 0
        pure (if click then Just p else choice)) Nothing (zip [0..] (take 4 projects))
      N.text window (x+24) (y+255) 13 "PROJECT FILE / FULL PATH" 1
      path'<-N.textbox window 900 (x+24) (y+281) 572 34 path
      browse<-N.button window (x+24) (y+329) 150 32 "Browse..." 0
      load<-N.button window (x+183) (y+329) 201 32 "Load project" 1
      cancel<-N.button window (x+393) (y+329) 203 32 "Cancel" 0
      browsed<-if browse then chooseProject else pure Nothing
      let chosen'=if load then Just path' else if isJust chosen then chosen else browsed
      case chosen' of
        Just p->if dirty s then pure s {modal=Just (Unsaved p)} else N.clearFocus window >> loadIntoEditor window p s
        Nothing->pure s {modal=if cancel then Nothing else Just (ProjectPicker path' (if nextPage then drop 4 projects++take 4 projects else projects))}
    Just (Unsaved next)->do
      N.text window (x+24) (y+24) 23 "Save your scene changes?" 0
      N.text window (x+24) (y+92) 16 "This project has unsaved edits." 1
      yes<-N.button window (x+24) (y+162) 174 36 "Save and load" 1
      discard<-N.button window (x+215) (y+162) 177 36 "Discard and load" 2
      cancel<-N.button window (x+409) (y+162) 187 36 "Cancel" 0
      if yes then do saved<-save s;if dirty saved then pure saved else loadIntoEditor window next saved
      else if discard then loadIntoEditor window next s else pure s {modal=if cancel then Nothing else modal s}

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

runPlayer :: N.Window -> [SomeGame] -> FilePath -> FilePath -> Project -> Maybe Int -> Maybe FilePath -> IO ()
runPlayer window registry _ root p limit capture = do
  (r,started,msg)<-startRuntimeWith registry (N.audio window) root p
  mapM_ putStrLn msg
  loop 0 r started 0 "" `finally` stopRuntime r
  where
    loop frame r current acc status = do
      quit<-N.shouldClose window
      escape<-N.key window 256 True
      if quit||escape||maybe False (frame>=) limit then do
        forM_ capture (N.screenshot window)
        stopRuntime r
      else do
        dt<-min 0.1 <$> N.frameTime window
        input<-N.inputs window
        (r',p',remaining,hud')<-step (min 0.1 (acc+dt)) r current status input
        (w,h)<-N.dimensions window
        N.begin window
        _<-N.drawScene window root p' (0,0,w,h) False False Nothing
        when (not (null hud')) $ do N.rect window 24 24 650 103 0;N.text window 40 41 22 hud' 0
        N.text window 26 (h-35) 14 "WASD / arrows move  |  Space jump  |  R restart  |  Esc quit" 1
        N.end window
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
placePrefab :: N.Window -> (Float,Float,Float,Float) -> Editor -> IO Editor
placePrefab window rect s=case pendingPrefab s of
  Just path | not (isPlaying s) && modal s==Nothing -> do
    mouse<-N.getMouse window
    if not (C.mouseEnabled mouse && C.leftPressed mouse && C.inside rect mouse) then pure s else do
      cam<-N.getCamera window
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
