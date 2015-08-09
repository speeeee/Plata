import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))
import System.Random (randomIO)

import Core.Physics
import Core.Hitbox

{-: reshape ( w h -- )
   [ 0 0 ] 2dip glViewport GL_PROJECTION glMatrixMode
   GL_PROJECTION glMatrixMode
   glLoadIdentity
   -30.0 30.0 -30.0 30.0 -30.0 30.0 glOrtho
   GL_MODELVIEW glMatrixMode ;-}

ln = Line (3,4) (30,4)
ln2 = Line (24,4) (24,10)
ln3 = Line (24,10) (30,10)
ln4 = Line (0,22) (10,22)

yLns = [Line (2,4) (30,4), Line (0,22) (10,22), Line (23,14) (30,14)]
xLns = [Line (23,4) (23,13.99)]

initGL win = do
  glShadeModel gl_SMOOTH
  glClearColor 0 0 0 0
  (w,h) <- K.getFramebufferSize win
  resizeScene win w h

resizeScene win w h = do
  glViewport 0 0 (fromIntegral w) (fromIntegral h)
  glMatrixMode gl_PROJECTION
  glLoadIdentity
  glOrtho (-30) 30 (-30) 30 (-30) 30
  glMatrixMode gl_MODELVIEW

drawScene player _ = do
  glClear $ fromIntegral $ gl_COLOR_BUFFER_BIT .|. gl_DEPTH_BUFFER_BIT
  let (x, y) = pos player
  glLoadIdentity
  --glTranslatef (-30) (-30) 0
  glTranslatef (-x) (-y) 0
  glBegin gl_QUADS
  glColor3f 1 1 1
  mapM_ (\(x,y,z) -> glVertex3f x y z) [(x,y,0),(x+2,y,0),(x+2,y+3.2,0),(x,y+3.2,0)]
  glColor3f 1 0 0
  mapM_ (\(x,y,z) -> glVertex3f x y z) [(4,0,0),(4,4,0),(30,4,0),(30,0,0),
                                        (25,4,0),(25,14,0),(30,14,0),(30,4,0)]
  glEnd

shutdown :: K.Window -> IO ()
shutdown win = do
  K.destroyWindow win
  K.terminate
  _ <- exitWith ExitSuccess
  return ()

isPressed :: K.KeyState -> Bool
isPressed K.KeyState'Pressed = True
isPressed K.KeyState'Repeating = True
isPressed _ = False

getInput :: K.Window -> IO (GLfloat, GLfloat, Bool)
getInput win = do
  x0 <- isPressed `fmap` K.getKey win K.Key'Left
  x1 <- isPressed `fmap` K.getKey win K.Key'Right
  y0 <- isPressed `fmap` K.getKey win K.Key'Down
  y1 <- isPressed `fmap` K.getKey win K.Key'Up
  j <- isPressed `fmap` K.getKey win K.Key'Space
  let x0n = if x0 then -1 else 0
      x1n = if x1 then 1 else 0
      y0n = if y0 then -1 else 0
      y1n = if y1 then 1 else 0
  return (x0n + x1n, y0n + y1n, j)

processInput :: Player -> Bool -> K.Window -> IO (Bool, Player)
processInput player lj win = do
  (xi, yi, j) <- liftIO $ getInput win
  let (x,y) = pos player
      (x',y') = (appVecs cos fv' x, appVecs sin fv' y)
      fv' =  {-addVect (FVector grav (3*pi/2) Gravity) $ applyDecay decay (rad player) $-}
             inputVectors (if (st player) == Air then (xi*accel)/10 else xi*accel) $ fv player
      {-p = pHitY Air (spawn player) (x,y) (x',y') ln grav fv'
      p' = pHitY (st p) (spawn player) (x,y) (pos p) ln3 grav (fv p)
      p'' = pHitY (st p') (spawn player) (x,y) (pos p') ln4 grav (fv p')
      p'' = testLinesY (x,y) (Player (spawn player) (x',y') 0 fv' Air) grav yLns-}
      p'' = testLinesX (x,y) (testLinesY (x,y) (Player (spawn player) (x',y') 0 fv' Air) grav yLns) xLns
  putStrLn (show fv')
  {-return (pHitX (st p'') (spawn player) (x,y) (pos p'') ln2
                (if st p'' == Air then addVect (FVector grav (3*pi/2) Gravity) (fv p'')
                                  else fv p''))-}
  --putStrLn $ show $ exstV JInput (fv p'')
  return (j, Player (spawn player) (fst $ pos p'', if (not lj) && j && st p'' == Ground then snd (pos p'')+0.5 else snd $ pos p'') 0
                    (chkJmps p'' lj j) (st p''))

--runGame player win = runGame' player win (0::Int)
runGame player lj win = do
  (j,altPlayer) <- processInput player lj win
  K.pollEvents
  drawScene altPlayer win
  K.swapBuffers win
  runGame altPlayer j win

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "plata" Nothing Nothing
  let player = Player (0,58) (0,58) 1 [(FVector 0 0 Input)] Air
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene player))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame player False win
