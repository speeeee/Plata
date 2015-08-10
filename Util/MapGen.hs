module Util.MapGen (rooms) where

import Graphics.Rendering.OpenGL.Raw
import System.Random (randomIO)
import Core.Hitbox

type Open = (Bool,Bool,Bool,Bool) --(N,S,E,W)

rooms =
  (([Line (-2,-2) (-2,2.99),Line (2,-2) (2,2.99),Line (4,-2) (4,3.99),
     Line (11,0) (11,3.99),Line (12,0) (12,2.99),Line (16,0) (16,2.99)],
    [Line (-2,3) (2,3),Line (4,4) (11,4),Line (12,3) (16,3)]),
   [(0,0)::(GLfloat,GLfloat),(0,3),(2,3),(2,0), (6,0),(6,4),(11,4),(11,0), (14,0),(14,3),(16,3),(16,0)])
{- ++++++++++++++++++
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +                +
   +      #####     +
   +##    #####   ##+
   +##    #####   ##+
   +##    #####   ##+
   ++++++++++++++++++ -}


