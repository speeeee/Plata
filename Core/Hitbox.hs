module Core.Hitbox (testLine,testLineY,testLineX,Line(..)) where

import Graphics.Rendering.OpenGL.Raw

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
