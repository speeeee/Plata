module Core.Hitbox (testLine,testLineY,testLineX,Line(..),pHitY,pHitX,Player(..),PState(..),
                    hbToLs,testLinesY,testLinesX,chkJmps) where

import Graphics.Rendering.OpenGL.Raw

import Core.Physics

data PState = Ground | Air | None deriving (Show,Eq)
data Player = Player { spawn :: (GLfloat,GLfloat),
                       pos :: (GLfloat,GLfloat),
                       rad :: GLfloat, fv :: [FVector],
                       st :: PState } deriving (Show,Eq)

data Line = Line { pta :: (GLfloat,GLfloat),
                   ptb :: (GLfloat,GLfloat) } deriving (Show,Eq)

data Hitbox = Hitbox { pa :: (GLfloat,GLfloat),
                       pb :: (GLfloat,GLfloat) } deriving (Show,Eq)

-- makes lines for side-specific hitboxes.
hbToLs hb = ([Line (fst $ pa hb, snd $ pa hb) (fst $ pb hb, snd $ pa hb),
              Line (fst $ pa hb, snd $ pb hb) (fst $ pb hb, snd $ pb hb)],
             [Line (fst $ pa hb, snd $ pa hb) (fst $ pa hb, snd $ pb hb),
              Line (fst $ pb hb, snd $ pa hb) (fst $ pb hb, snd $ pb hb)])

testLinesY o p grav = foldl (hitY o grav) p
hitY o grav p ln = pHitY (st p) (spawn p) o (fst $ pos p, snd $ pos p) ln grav (fv p)

testLinesX o = foldl (hitX o)
hitX o p ln = pHitX (st p) (spawn p) o (fst $ pos p, snd $ pos p) ln (fv p)

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
  then Player sp (appVecs cos fv' x,snd $ pta ln) 0 fv' Ground
  else Player sp (x',y') 0
       {-(addVect (FVector grav (3*pi/2) Gravity) fv)-} fv tf


pHitX tf sp (x,y) (x',y') ln fv =
  if testLineX (x,y) (x',y') ln
  then Player sp ((fst $ pta ln)-0.001,y') 0 (chHF fv) tf
  else Player sp (x',y') 0 fv tf

chkJmps p lj j =
  if st p == Air then if exstV JInput (fv p) && not j
    then opVect (*) (FVector 0.95 (pi/2) JInput) $ addVect (FVector grav (3*pi/2) Gravity) (fv p)
    else addVect (FVector grav (3*pi/2) Gravity) (fv p)
  else if j then addVect (FVector 0.7 (pi/2) JInput) (fv p)
  else fv p
