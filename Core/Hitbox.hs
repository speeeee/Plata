module Core.Hitbox (testLine,testLineY,testLineX,Line(..),pHitY,pHitX,Player(..)) where

import Graphics.Rendering.OpenGL.Raw

import Core.Physics


data Player = Player { spawn :: (GLfloat,GLfloat),
                       pos :: (GLfloat,GLfloat),
                       rad :: GLfloat,
                       fv :: [FVector] } deriving (Show,Eq)

data Line = Line { pta :: (GLfloat,GLfloat),
                   ptb :: (GLfloat,GLfloat) } deriving (Show,Eq)

-- will only work with points with slope = [real] or slope = infinity.
-- `xy' works as a test for which slope.
testLine p p' xy bnd l =
  let (b,b') = (xy $ pta l, xy $ ptb l)
      (a,a') = (xy p, xy p') in
  bnd p' >= bnd (pta l) && bnd p' <= bnd (ptb l) &&
  (a == b || (a < b) == (a' > b) || (a > b) == (a' < b))

testLineY p p' = testLine p p' snd fst
testLineX p p' = testLine p p' fst snd

pHitY tf sp (x,y) (x',y') ln grav fv =
  let fv' = applyDecay decay 0 $ remVF fv in
  if testLineY (x,y) (x',y') ln
  then (True, Player sp (appVecs cos fv' x,snd $ pta ln) 0 fv')
  else (tf, Player sp (x',y') 0
            {-(addVect (FVector grav (3*pi/2) Gravity) fv)-} fv)


pHitX sp (x,y) (x',y') ln fv =
  if testLineX (x,y) (x',y') ln
  then Player sp ((fst $ pta ln)-0.001,y') 0 fv
  else Player sp (x',y') 0 fv
