module Main where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit
import Graphics.UI.GLUT.Callbacks.Window


import System.Random
import System.IO.Unsafe

import Mapa

type CorMain = (GLfloat, GLfloat, GLfloat)

corMain :: GLfloat -> GLfloat -> GLfloat -> CorMain
corMain r g b = (r, g, b)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    createWindow "Hello Tste" -- Titulo da janela
    -- mapa <- newIORef 1
    mapa <- newIORef []
    displayCallback $= (display mapa)
    -- update rotation
    cd <- randomMapa 20 20
    -- putStrLn (show cd)
    mapa $= getCores cd
    -- putStrLn "main"
    attachMyKeyboardMouseCallback
    mainLoop



cores :: [CorMain]
cores = [
    corMain 0 0 0,
    corMain 1 0 0,
    corMain 0 1 0,
    corMain 0 0 1,
    corMain 1 1 1
    ] 

    
getCores:: [[Int]] -> [[CorMain]]
getCores [] = []
getCores (linha:ls) = [ cores!!c | c <- linha ] : getCores ls


-- Gerar uma lista de numeros entre 0 e 4
randomLinha :: Int -> IO([Int])
randomLinha 0 = return []
randomLinha n = do
    r  <- randomRIO (0,4)
    rs <- randomLinha (n-1)
    return (r:rs) 

randomMapa :: Int -> Int -> IO [[Int]]
randomMapa 0 l = return []
randomMapa n l = do
    linha <- randomLinha l
    mapa <- randomMapa (n-1) l
    return (linha:mapa) 














display :: IORef [[CorMain]] -> IO ()
display atualizar = do
    clear [ ColorBuffer]
    loadIdentity
    -- rotation' <- get rotation
    -- rotate rotation' $ Vector3 0 0 (1::GLfloat)
    -- renderPrimitive Triangles $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
    -- displayCubo
    mapa atualizar
    swapBuffers

-- update :: IO ()
-- update = do
    -- a <- get rotation
    -- rotation $= a + 1
    -- postRedisplay Nothing
    -- addTimerCallback 800 $ update rotation

myKeyboardMouseCallback key keyState modifiers position =
  case (key, keyState) of
    (SpecialKey KeyRight, Up) -> postRedisplay Nothing
    (SpecialKey KeyLeft, Up) -> postRedisplay Nothing
    (SpecialKey KeyUp, Up) -> postRedisplay Nothing
    (SpecialKey KeyDown, Up) -> postRedisplay Nothing

    _ -> return () -- ignore other buttons

attachMyKeyboardMouseCallback = keyboardMouseCallback $= Just myKeyboardMouseCallback

-- myPoints :: [(GLfloat, GLfloat, GLfloat)]
-- myPoints = [(sin(2*pi * k / 3), cos(2*pi * k / 3), 0.0) | k <- [1..3]]


teste :: IORef [[CorMain]] -> IO ()
teste mapa = do
    m <- get mapa
    putStrLn (show m)
    postRedisplay Nothing