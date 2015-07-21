import qualified Graphics.UI.GLFW as K
import Graphics.Rendering.OpenGL.Raw
import Graphics.Rendering.GLU.Raw (gluPerspective)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay)
import Data.Bits ( (.|.) )
import System.Exit (exitWith, ExitCode(..))

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
  glTranslatef (-30) (-30) 0
  glBegin gl_QUADS
  glColor3f 1 1 1
  mapM_ (\(x,y,z) -> glVertex3f x y z) [(x,y,0),(x+1,y,0),(x+1,y+1.6,0),(x,y+1.6,0)]
  glColor3f 1 0 0
  mapM_ (\(x,y,z) -> glVertex3f x y z) [(4,0,0),(4,4,0),(30,4,0),(30,0,0),
                                        (25,4,0),(25,10,0),(30,10,0),(30,4,0)]
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

getInput :: K.Window -> IO (GLfloat, GLfloat)
getInput win = do
  x0 <- isPressed `fmap` K.getKey win K.Key'Left
  x1 <- isPressed `fmap` K.getKey win K.Key'Right
  y0 <- isPressed `fmap` K.getKey win K.Key'Down
  y1 <- isPressed `fmap` K.getKey win K.Key'Up
  let x0n = if x0 then -1 else 0
      x1n = if x1 then 1 else 0
      y0n = if y0 then -1 else 0
      y1n = if y1 then 1 else 0
  return (x0n + x1n, y0n + y1n)

processInput :: Player -> K.Window -> IO Player
processInput player win = do
  (xi, yi) <- liftIO $ getInput win
  let (x,y) = pos player
      (x',y') = (appVecs cos fv' x, appVecs sin fv' y)
      fv' =  {-addVect (FVector grav (3*pi/2) Gravity) $ applyDecay decay (rad player) $-}
             inputVectors (xi*accel) $ fv player
      p = pHitY (spawn player) (x,y) (x',y') ln fv'
      --p' = pHitY (spawn player) (x,y) (pos p) ln3 (fv p)
  putStrLn (show fv')
  return (pHitX (spawn player) (x,y) (pos p) ln2 (fv p))

runGame player win = runGame' player win (0::Int)
runGame' player win acc = do
  altPlayer <- processInput player win
  K.pollEvents
  drawScene altPlayer win
  K.swapBuffers win
  runGame' altPlayer win (acc + 1)

main = do
  True <- K.init
  Just win <- K.createWindow 1280 800 "plata" Nothing Nothing
  let player = Player (0,58) (0,58) 1 [(FVector 0 0 Input)]
  K.makeContextCurrent (Just win)
  K.setWindowRefreshCallback win (Just (drawScene player))
  K.setFramebufferSizeCallback win (Just resizeScene)
  K.setWindowCloseCallback win (Just shutdown)
  initGL win
  runGame player win
