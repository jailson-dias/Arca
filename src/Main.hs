module Main where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit
import Graphics.UI.GLUT.Callbacks.Window

import Mapa

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello Tste" -- Titulo da janela
    rotation <- newIORef 0.0
    displayCallback $= (display rotation)
    update rotation
    attachMyKeyboardMouseCallback
    mainLoop

display :: IORef GLfloat -> IO ()
display rotation = do
    clear [ ColorBuffer]
    loadIdentity
    -- rotation' <- get rotation
    -- rotate rotation' $ Vector3 0 0 (1::GLfloat)
    -- renderPrimitive Triangles $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    -- displayCubo
    mapa
    swapBuffers

update :: IORef GLfloat -> IO ()
update rotation = do
    a <- get rotation
    rotation $= a + 1
    postRedisplay Nothing
    addTimerCallback 16 $ update rotation

myKeyboardMouseCallback key keyState modifiers position =
  case (key, keyState) of
    (SpecialKey KeyRight, Up) -> putStrLn("Right")
    (SpecialKey KeyLeft, Up) -> putStrLn("Left")
    (SpecialKey KeyUp, Up) -> putStrLn("Up")
    (SpecialKey KeyDown, Up) -> putStrLn("Down")

    _ -> return () -- ignore other buttons

attachMyKeyboardMouseCallback = keyboardMouseCallback $= Just myKeyboardMouseCallback

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin(2*pi * k / 3), cos(2*pi * k / 3), 0.0) | k <- [1..3]]
