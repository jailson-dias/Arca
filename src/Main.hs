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
import Environment


type Heroi = ((Int, Int), Int)
type Posicao = (Int, Int)
type CorMain = (GLfloat,GLfloat,GLfloat)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 700 600
    createWindow "Hello Tste" -- Titulo da janela
    heroi <- newIORef ((19::Int, 19::Int), 300::Int)
    corHeroi <- newIORef (1::GLfloat,0.5::GLfloat,0::GLfloat)
    corCasa <- newIORef ("",5::Int,(1::GLfloat,0.5::GLfloat,0::GLfloat))

    mapa <- newIORef []
    cd <- randomMapa 20 20 35 65 85 95 100
    (h, _) <- get heroi
    c <- get corHeroi
    let (ma, co) = setCasa (getObjetos cd) h c
    mapa $= ma
    corCasa $= co

    displayCallback $= (display mapa)
    attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi
    mainLoop






-- Gerar uma lista de numeros entre 0 e 4
-- Gerar uma lista de numeros entre 0 e 4
funcao :: Int -> Int -> Int -> Int -> Int -> Int -> Int
funcao chao espinho flecha buraco chamas n
 | n >= 0 && n <= chao = 0
 | n > chao && n <= espinho = 1
 | n > espinho && n <= flecha = 2
 | n > flecha && n <= buraco = 3
 | n > buraco && n <= chamas = 4

randomLinha :: Int -> IO [Int]
randomLinha 0 = return []
randomLinha n = do
    r  <- randomRIO (0,100)
    rs <- randomLinha (n-1)
    return (r:rs) 

randomMapa :: Int -> Int -> Int -> Int -> Int -> Int -> Int -> IO [[Int]]
randomMapa 0 l _ _ _ _ _= return []
randomMapa n l chao espinho flecha buraco chamas = do
    linha <- randomLinha l
    mapa <- randomMapa (n-1) l chao espinho flecha buraco chamas
    return (map (funcao chao espinho flecha buraco chamas) linha : mapa) 


-- randomLinha :: Int -> IO([Int])
-- randomLinha 0 = return []
-- randomLinha n = do
--     r  <- randomRIO (0,4)
--     rs <- randomLinha (n-1)
--     return (r:rs) 

-- randomMapa :: Int -> Int -> IO [[Int]]
-- randomMapa 0 l = return []
-- randomMapa n l = do
--     linha <- randomLinha l
--     mapa <- randomMapa (n-1) l
--     return (linha:mapa) 


display :: IORef [[Objeto]] -> IO ()
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
setCasa :: [[Objeto]] -> Posicao -> CorMain -> ([[Objeto]], Objeto)
setCasa mapa (x, y) cor = (take x mapa ++ [take y (mapa!!x) ++ [(\(nome, dano, cor) nova -> (nome,dano, nova)) (mapa!!x!!y) cor] ++ drop (y+1) (mapa!!x)] ++ drop (x+1) mapa, mapa!!x!!y)

-- Movimento para cima
-- moveKeyUp :: IORef [[CorMain]] -> IORef Posicao -> IORef CorMain -> IORef CorMain -> IO ()
-- moveKeyUp mapa heroi corCasa corHeroi = do
--     m <- get mapa
--     (x,y) <- get heroi
--     c <- get corCasa
--     ch <- get corHeroi
--     putStrLn ("Up")
--     if x > 0 then do
--             let (ma, _) = setCasa m (x,y) c
--             let (map, co) = setCasa ma (x-1,y) ch
--             corCasa $= co
--             mapa $= map
--             heroi $= (x-1,y)
--     else 
--         putStrLn "else"
--     postRedisplay Nothing

keyUp :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IO ()
keyUp mapa heroi corCasa corHeroi = do
    m <- get mapa
    ((x,y), vida) <- get heroi
    (_,_,c) <- get corCasa
    ch <- get corHeroi
    -- putStrLn ("Up")
    if x > 0 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, (nome, d, cor)) = setCasa ma (x-1,y) ch
            corCasa $= (nome, d, cor)
            mapa $= map
            heroi $= ((x-1,y), vida - d)
    else 
        putStr ""
    ((x2,y2), vida2) <- get heroi
    putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
    postRedisplay Nothing

-- Movimento para baixo
keyDown :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IO ()
keyDown mapa heroi corCasa corHeroi = do
    m <- get mapa
    ((x,y), vida) <- get heroi
    (_,_,c) <- get corCasa
    ch <- get corHeroi
    -- putStrLn ("Down")
    if x < 19 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, (nome, d, cor)) = setCasa ma (x+1,y) ch
            corCasa $= (nome, d, cor)
            mapa $= map
            heroi $= ((x+1,y), vida - d)
    else 
        putStr ""
    ((x2,y2), vida2) <- get heroi
    putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
    postRedisplay Nothing

-- Movimento para esquerda
keyLeft :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IO ()
keyLeft mapa heroi corCasa corHeroi = do
    m <- get mapa
    ((x,y), vida) <- get heroi
    (_,_,c) <- get corCasa
    ch <- get corHeroi
    -- putStrLn ("Left")
    if y > 0 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, (nome, d, cor)) = setCasa ma (x,y-1) ch
            corCasa $= (nome, d, cor)
            mapa $= map
            heroi $= ((x,y-1), vida - d)
    else 
        putStr ""
    ((x2,y2), vida2) <- get heroi
    putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
    postRedisplay Nothing

-- Movimento para direita
keyRight :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IO ()
keyRight mapa heroi corCasa corHeroi = do
    m <- get mapa
    ((x,y), vida) <- get heroi
    (_,_,c) <- get corCasa
    ch <- get corHeroi
    -- putStrLn ("Right")
    if y < 19 then do
            let (ma, _) = setCasa m (x,y) c
            let (map, (nome, d, cor)) = setCasa ma (x,y+1) ch
            corCasa $= (nome, d, cor)
            mapa $= map
            heroi $= ((x,y+1), vida - d)
    else 
        putStr ""
    ((x2,y2), vida2) <- get heroi
    putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
    postRedisplay Nothing

