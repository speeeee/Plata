module Util.MapGen (rooms) where

import Graphics.Rendering.OpenGL.Raw
import System.Random (randomIO)
import Core.Hitbox

type Open = (Bool,Bool,Bool,Bool) --(N,S,E,W)

rooms =
  (([xLn (-2) (-2) 2.99,xLn 2 (-2) 2.99,xLn 4 (-2) 3.99,
     xLn 11 0 3.99,xLn 12 0 2.99,xLn 16 0 2.99],
    [yLn 3 (-2) 2,yLn 4 4 11,yLn 3 12 16]),
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


