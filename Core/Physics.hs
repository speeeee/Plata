module Core.Physics (FVector(..), FVType(..), addVect, appVecs, applyDecay, cleanUp, inputVectors, remVF,
                     chHF,remByTyp,accel,grav,exstV,decay,opVect) where

import Graphics.Rendering.OpenGL.Raw
import Data.List

data FVType = Input | Collision | Gravity | JInput deriving (Eq, Show)
data FVector = FVector { f :: GLfloat,
                         dir :: GLfloat, -- degrees from east; counter-clockwise
                         typ :: FVType } deriving (Show,Eq)

accel = (0.045::GLfloat) -- base speed of acceleration
decay = (0.15::GLfloat) -- base decay of Input force vectors.
grav = (0.025::GLfloat)

--chk = (\x -> (typ x) == GravityInit)

inputVectors xt fv =
  let d = find (\x -> (dir x) == 0) fv in
  case d of
       Just a -> (FVector ((f a) + xt) (dir a) (typ a)):(filter (\x -> (dir x) /= (dir a)) fv)
       Nothing -> fv

appVecs op fv l = foldr (+) l (map (\x -> (op $ dir x)*(f x)) fv)

cleanUp fv = map (\x -> FVector (if (f x) < 0 then 0 else (f x)) (dir x) (typ x)) fv
applyDecay d r fv =
  let ri = find (\y -> (dir y) == 0) fv
      fv' = filter (\x -> (dir x) /= 0) fv in
  case ri of
  Just r' ->
    --if (f r') > (f l') then (FVector ((f l') + d*(f r')) (dir l') (typ l')):r':fv'
    --else if (f l') > (f r') then (FVector ((f r') + d*(f l')) (dir r') (typ r')):l':fv'
    --else fv
    if (f r') /= 0 then (FVector ((f r') - d*(f r')) (dir r') (typ r')):fv'
    else fv
  Nothing -> fv

addVect v fv =
  case find (\x -> (dir x) == (dir v)) fv of
       Just a -> (FVector ((f a) + (f v)) (dir a) (typ a)):(filter (\x -> (dir x) /= (dir v)) fv)
       Nothing -> v:fv
opVect o v = map (\x -> if (dir x) == (dir v) then FVector ((f x) `o` (f v)) (dir x) (typ x) else x)

-- remove vector by direction
remVect d = filter (/= d)
remByTyp t = filter (\x -> (typ x) /= t)

remVF fv = [FVector (foldr (+) 0 (map (\x -> (cos $ dir x)*(f x)) fv)) 0 Input]
chHF = --map (\x -> FVector ((cos $ dir x)*(f x)/(3)+(sin $ dir x)*(f x)) (dir x) (typ x))
       map (\x -> if typ x == Input then FVector ((dir x)/(-3)) (dir x) (typ x) else x)
--remVF fv = [FVector (foldr (+) 0 (map (\x -> (cos $ dir x)*(f x)) fv)) 0 Input,
--            FVector (foldr (-) 0 (map (\y -> (sin $ dir y)*(f y)/3) fv)) (pi/2) Collision]

exstV t = any (\k -> ((==) . typ) k t)
