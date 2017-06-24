module Main where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello Tste" -- Titulo da janela
    rotation <- newIORef 0.0
    displayCallback $= (display rotation)
    update rotation
    mainLoop

cube :: IO ()
cube = do
    let color3f r g b = color $ Color3 r g (b :: GLfloat)
        vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)
    clear [ColorBuffer]
    renderPrimitive Quads $ do
        color3f 1 0 0
        vertex3f 0 0 0
        vertex3f 0 0.95 0
        vertex3f 1 1 0
        vertex3f 1 0 0
    flush

display :: IORef GLfloat -> IO ()
display rotation = do
    clear [ ColorBuffer]
    loadIdentity
    -- rotation' <- get rotation
    -- rotate rotation' $ Vector3 0 0 (1::GLfloat)
    -- renderPrimitive Triangles $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    -- displayCubo
    cube
    swapBuffers

update :: IORef GLfloat -> IO ()
update rotation = do
    a <- get rotation
    rotation $= a + 1
    postRedisplay Nothing
    addTimerCallback 16 $ update rotation

myPoints :: [(GLfloat, GLfloat, GLfloat)]
myPoints = [(sin(2*pi * k / 3), cos(2*pi * k / 3), 0.0) | k <- [1..3]]
