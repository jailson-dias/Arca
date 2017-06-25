module Main where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit
import Graphics.UI.GLUT.Callbacks.Window
-- import Graphics.UI.GLUT.Window
-- import Graphics.Rendering.OpenGL.GL.CoordTrans ( Position(..), Size(..) )


import System.Random
import System.IO.Unsafe

import Mapa

type CorMain = (GLfloat, GLfloat, GLfloat)

type Posicao = (Int, Int)

corMain :: GLfloat -> GLfloat -> GLfloat -> CorMain
corMain r g b = (r, g, b)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 700 600
    createWindow "Hello Tste" -- Titulo da janela
    heroi <- newIORef (19::Int, 19::Int)
    corHeroi <- newIORef (1::GLfloat,0.5::GLfloat,0::GLfloat)
    corCasa <- newIORef (1::GLfloat,0.5::GLfloat,0::GLfloat)

    mapa <- newIORef []
    cd <- randomMapa 20 20

    h <- get heroi
    c <- get corHeroi
    let (ma, co) = setCasa (getCores cd) h c
    mapa $= ma
    corCasa $= co

    displayCallback $= (display mapa)
    attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi
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
    mapa atualizar
    swapBuffers


myKeyboardMouseCallback mapa heroi corCasa corHeroi key keyState modifiers position =
    -- keyboardMouseCallback 
  case (key, keyState) of
    (SpecialKey KeyRight, Up) -> keyRight mapa heroi corCasa corHeroi
    (SpecialKey KeyLeft, Up) -> keyLeft mapa heroi corCasa corHeroi
    (SpecialKey KeyUp, Up) -> keyUp mapa heroi corCasa corHeroi
    (SpecialKey KeyDown, Up) -> keyDown mapa heroi corCasa corHeroi

    _ -> return () -- ignore other buttons

attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi = keyboardMouseCallback $= Just (myKeyboardMouseCallback mapa heroi corCasa corHeroi)


-- Alterar a cor de uma casa no mapa
setCasa :: [[CorMain]] -> Posicao -> CorMain -> ([[CorMain]], CorMain)
setCasa mapa (x, y) cor = (take x mapa ++ [take y (mapa!!x) ++ [cor] ++ drop (y+1) (mapa!!x)] ++ drop (x+1) mapa, mapa!!x!!y)

-- Movimento para cima
keyUp :: IORef [[CorMain]] -> IORef Posicao -> IORef CorMain -> IORef CorMain -> IO ()
keyUp mapa heroi corCasa corHeroi = do
    m <- get mapa
    (x,y) <- get heroi
    c <- get corCasa
    ch <- get corHeroi
    putStrLn ("Up")
    if x > 0 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, co) = setCasa ma (x-1,y) ch
            corCasa $= co
            mapa $= map
            heroi $= (x-1,y)
    else 
        putStrLn "else"
    postRedisplay Nothing

-- Movimento para baixo
keyDown :: IORef [[CorMain]] -> IORef Posicao -> IORef CorMain -> IORef CorMain -> IO ()
keyDown mapa heroi corCasa corHeroi = do
    m <- get mapa
    (x,y) <- get heroi
    c <- get corCasa
    ch <- get corHeroi
    putStrLn ("Down")
    if x < 19 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, co) = setCasa ma (x+1,y) ch
            corCasa $= co
            mapa $= map
            heroi $= (x+1,y)
    else 
        putStrLn "else"
    postRedisplay Nothing

-- Movimento para esquerda
keyLeft :: IORef [[CorMain]] -> IORef Posicao -> IORef CorMain -> IORef CorMain -> IO ()
keyLeft mapa heroi corCasa corHeroi = do
    m <- get mapa
    (x,y) <- get heroi
    c <- get corCasa
    ch <- get corHeroi
    putStrLn ("Left")
    if y > 0 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, co) = setCasa ma (x,y-1) ch
            corCasa $= co
            mapa $= map
            heroi $= (x,y-1)
    else 
        putStrLn "else"
    postRedisplay Nothing

-- Movimento para direita
keyRight :: IORef [[CorMain]] -> IORef Posicao -> IORef CorMain -> IORef CorMain -> IO ()
keyRight mapa heroi corCasa corHeroi = do
    m <- get mapa
    (x,y) <- get heroi
    c <- get corCasa
    ch <- get corHeroi
    putStrLn ("Right")
    if y < 19 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, co) = setCasa ma (x,y+1) ch
            corCasa $= co
            mapa $= map
            heroi $= (x,y+1)
    else 
        putStrLn "else"
    postRedisplay Nothing








    {--

        Int->Int->Int
        Int->Bool


        --}