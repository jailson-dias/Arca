module Main where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit

import Mapa

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello Tste" -- Titulo da janela
    rotation <- newIORef 0.0
    displayCallback $= (display rotation)
    update rotation
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

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin(2*pi * k / 3), cos(2*pi * k / 3), 0.0) | k <- [1..3]]
