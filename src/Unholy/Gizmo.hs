module Unholy.Gizmo where
import qualified Unholy.Types as T
import Unholy.Transform (rotationQ,quaternionEuler)
import Unholy.Renderer.Camera
import qualified Linear as L
import Data.List (sortOn)

data Mode = Move | Rotate | Scale deriving (Eq,Show)
data Drag = Drag {dragEntity::T.Entity,dragMode::Mode,dragAxis::Int,dragPose::T.Transform,dragMouse::L.V2 Float} deriving Show
data Edit = Edit {editEntity::T.Entity,editTransform::T.Transform,editStarted::Bool,editEnded::Bool} deriving Show
axes :: [L.V3 Float]
axes=[L.V3 1 0 0,L.V3 0 1 0,L.V3 0 0 1]
handleSize :: (Float,Float,Float,Float) -> T.Camera -> T.Transform -> Float
handleSize (_,_,_,h) cam pose=max 0.03 (L.distance (toLinear (T.eye cam)) (toLinear (T.position pose))*2*tan (T.fov cam*pi/360)*72/max 1 h)
projectPoint :: (Float,Float,Float,Float) -> T.Camera -> L.V3 Float -> Maybe (L.V2 Float)
projectPoint (x,y,w,h) cam (L.V3 a b c)
  | depth<=0.01 = Nothing
  | otherwise = Just (L.V2 (x+(px/depth+1)*w/2) (y+(1-py/depth)*h/2))
  where L.V4 px py _ depth=viewProjection (w/h) cam L.!* L.V4 a b c 1
axisVector :: Mode -> T.Transform -> Int -> L.V3 Float
axisVector mode pose i=if mode==Scale then L.rotate (rotationQ (T.rotation pose)) (axes!!i) else axes!!i
ringPoints :: Int -> [L.V3 Float]
ringPoints axis=[point (fromIntegral i*2*pi/64) | i<-[0..64::Int]]
  where point angle=cos angle L.*^ (axes!!((axis+1) `mod` 3))+sin angle L.*^ (axes!!((axis+2) `mod` 3))
segments :: Mode -> T.Transform -> Float -> Int -> [(L.V3 Float,L.V3 Float)]
segments mode pose size axis
  | mode==Rotate = zip points (drop 1 points)
  | otherwise = [(origin+0.18*size L.*^dir,origin+size L.*^dir)]
  where origin=toLinear (T.position pose);dir=axisVector mode pose axis
        points=map (\v->origin+size L.*^v) (ringPoints axis)
segmentDistance :: L.V2 Float -> L.V2 Float -> L.V2 Float -> Float
segmentDistance p a b=L.distance p (a+clamp 0 1 (L.dot (p-a) d/max 0.0001 (L.quadrance d)) L.*^d)
  where d=b-a
hitHandle :: (Float,Float,Float,Float) -> T.Camera -> Mode -> T.Transform -> Mouse -> Maybe Int
hitHandle rect cam mode pose mouse=case sortOn fst hits of (distance,axis):_ | distance<9 -> Just axis;_->Nothing
  where size=handleSize rect cam pose;point=L.V2 (mouseX mouse) (mouseY mouse)
        hits=[(minimum distances,axis) | axis<-[0..2],let distances=[segmentDistance point a b | (u,v)<-segments mode pose size axis,Just a<-[projectPoint rect cam u],Just b<-[projectPoint rect cam v],L.distance a b>2],not (null distances)]
update :: (Float,Float,Float,Float) -> T.Camera -> Mouse -> Bool -> Bool -> Maybe Mode -> Maybe (T.Entity,T.Transform) -> Maybe Drag -> (Maybe Drag,Maybe Edit,Bool)
update rect cam mouse down snap mode selected previous = case (mode,selected,previous) of
  (Just tool,Just (e,_),Just drag) | dragEntity drag==e && dragMode drag==tool ->
    let pose=applyDrag rect cam mouse snap drag
    in (if down then Just drag else Nothing,Just (Edit e pose False (not down)),True)
  (Just tool,Just (e,pose),_) | mouseEnabled mouse && leftPressed mouse && inside rect mouse ->
    case hitHandle rect cam tool pose mouse of
      Just axis -> (Just (Drag e tool axis pose (L.V2 (mouseX mouse) (mouseY mouse))),Just (Edit e pose True False),True)
      Nothing -> (Nothing,Nothing,False)
  _ -> (Nothing,Nothing,False)
applyDrag :: (Float,Float,Float,Float) -> T.Camera -> Mouse -> Bool -> Drag -> T.Transform
applyDrag rect cam mouse snap drag = case dragMode drag of
  Move -> pose {T.position=fromLinear (origin+snapped 0.25 delta L.*^ axis)}
  Scale -> let values=T.vlist (T.scale pose);factor=max 0.01 (1+snapped 0.1 (delta/size))
           in pose {T.scale=T.fromListV [if i==dragAxis drag then max 0.001 (v*factor) else v | (i,v)<-zip [0..] values]}
  Rotate -> pose {T.rotation=quaternionEuler (L.axisAngle axis (snapped 15 angle*pi/180)*rotationQ (T.rotation pose))}
  where
    pose=dragPose drag;origin=toLinear (T.position pose);size=handleSize rect cam pose
    axis=axisVector (dragMode drag) pose (dragAxis drag)
    point=L.V2 (mouseX mouse) (mouseY mouse);start=dragMouse drag
    center=maybe start id (projectPoint rect cam origin)
    endpoint=maybe center id (projectPoint rect cam (origin+size L.*^axis))
    screenAxis=endpoint-center
    delta=L.dot (point-start) screenAxis/max 0.0001 (L.quadrance screenAxis)*size
    -- Intersect the rotation plane to obtain an angle in world space. The
    -- screen angle fallback keeps nearly edge-on rings usable.
    planePoint m=let Ray p d=screenRay rect cam m;denominator=L.dot d axis
                 in if abs denominator<0.0001 then Nothing else Just (p+(L.dot (origin-p) axis/denominator) L.*^d-origin)
    startMouse=mouse {mouseX=case start of L.V2 a _->a,mouseY=case start of L.V2 _ b->b}
    angle=case (planePoint startMouse,planePoint mouse) of
      (Just a,Just b) | L.norm a>0.001 && L.norm b>0.001 -> atan2 (L.dot axis (L.cross a b)) (L.dot a b)*180/pi
      _ -> let L.V2 ax ay=start-center;L.V2 bx by=point-center
           in atan2 (ax*by-ay*bx) (ax*bx+ay*by)*180/pi
    snapped step value=if snap then fromIntegral (round (value/step)::Int)*step else value
