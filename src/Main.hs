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
import GameOver (game)
import Ganhou (ganhou)

type Heroi = ((Int, Int), Int)
type Posicao = (Int, Int)
type CorMain = (GLfloat,GLfloat,GLfloat)
type Nivel = (Char)
type ConfiguracaoNivel = (Int, Int, Int, Int, Int)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 700 600
    createWindow "Hello Tste" -- Titulo da janela
    heroi <- newIORef ((19::Int, 19::Int), 1500::Int)
    corHeroi <- newIORef (1::GLfloat,0.5::GLfloat,0::GLfloat)
    corCasa <- newIORef ("",5::Int,(1::GLfloat,0.5::GLfloat,0::GLfloat))
    venceu <- newIORef (1::Int)

    nivelJogo <- newIORef ('0'::Char)
    configuracaoNivel <- newIORef (0::Int, 0::Int, 0::Int, 0::Int, 0::Int)
    escolhaNivel nivelJogo configuracaoNivel

    mapa <- newIORef []
    cd <- randomMapa 20 20 configuracaoNivel
    (h, _) <- get heroi
    c <- get corHeroi
    let (ma, co) = setCasa (getObjetos cd) h c
    mapa $= ma
    corCasa $= co

    displayCallback $= (display mapa heroi venceu)
    attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi venceu
    mainLoop

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

randomMapa :: Int -> Int -> IORef ConfiguracaoNivel -> IO [[Int]]
randomMapa 0 l _ = return []
randomMapa n l configNivel = do
    (chao, espinho, flecha, buraco, chamas) <- get configNivel
    linha <- randomLinha l
    mapa <- randomMapa (n-1) l configNivel
    return (map (funcao chao espinho flecha buraco chamas) linha : mapa) 


display :: IORef [[Objeto]] -> IORef Heroi -> IORef Int -> IO ()
display atualizar heroi venceu = do
    (_, vida) <- get heroi
    v <- get venceu
    clear [ ColorBuffer]
    loadIdentity

    case v of
        2 -> ganhou
        1 -> mapa atualizar
        _ -> game
    -- if vida > 0 then
    --     mapa atualizar
    -- else
    --     game
    swapBuffers

myKeyboardMouseCallback mapa heroi corCasa corHeroi venceu key keyState modifiers position =
  case (key, keyState) of
    (SpecialKey KeyRight, Up) -> keyRight mapa heroi corCasa corHeroi venceu 
    (SpecialKey KeyLeft, Up) -> keyLeft mapa heroi corCasa corHeroi venceu
    (SpecialKey KeyUp, Up) -> keyUp mapa heroi corCasa corHeroi venceu
    (SpecialKey KeyDown, Up) -> keyDown mapa heroi corCasa corHeroi venceu

    _ -> return () -- ignore other buttons

attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi venceu = keyboardMouseCallback $= Just (myKeyboardMouseCallback mapa heroi corCasa corHeroi venceu)

--usuario escolhe o nivel do jogo [1,2,3]
escolhaNivel :: IORef Nivel -> IORef ConfiguracaoNivel -> IO()
escolhaNivel nivelJogo configuracaoNivel = do
     nivel <- getChar

     case nivel of
         '1' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (50, 75, 89, 97, 100) --porcentagem somada uma a uma até 100%
             return()
         '2' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (38, 66, 85, 94, 100)
             return()
         '3' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (28, 55, 75, 88, 100)
             return()
         x | x >= ' ' -> do
             escolhaNivel nivelJogo configuracaoNivel
         _ -> do
             putStrLn("Nível escolhido inválido.")
             escolhaNivel nivelJogo configuracaoNivel


-- Alterar a cor de uma casa no mapa
setCasa :: [[Objeto]] -> Posicao -> CorMain -> ([[Objeto]], Objeto)
setCasa mapa (x, y) cor = (take x mapa ++ [take y (mapa!!x) ++ [(\(nome, dano, cor) nova -> (nome,dano, nova)) (mapa!!x!!y) cor] ++ drop (y+1) (mapa!!x)] ++ drop (x+1) mapa, mapa!!x!!y)


keyUp :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IO ()
keyUp mapa heroi corCasa corHeroi venceu = do
    v <- get venceu
    if v == 1 then do
        m <- get mapa
        ((x,y), vida) <- get heroi
        (_,_,c) <- get corCasa
        ch <- get corHeroi
        if x > 0 then do
                let (ma, _) = setCasa m (x,y) c
                let (map, (nome, d, cor)) = setCasa ma (x-1,y) ch
                corCasa $= (nome, d, cor)
                mapa $= map
                heroi $= ((x-1,y), vida - d)
        else 
            putStr ""
        ((x2,y2), vida2) <- get heroi
        if vida <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then 
                venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

-- Movimento para baixo
keyDown :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IO ()
keyDown mapa heroi corCasa corHeroi venceu = do
    v <- get venceu
    if v == 1 then do
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
        if vida <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then 
                venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else 
        putStr ""

-- Movimento para esquerda
keyLeft :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IO ()
keyLeft mapa heroi corCasa corHeroi venceu = do
    v <- get venceu
    if v == 1 then do
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
        if vida <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then 
                venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

-- Movimento para direita
keyRight :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IO ()
keyRight mapa heroi corCasa corHeroi venceu = do
    v <- get venceu
    if v == 1 then do
        m <- get mapa
        ((x,y), vida) <- get heroi
        (_,_,c) <- get corCasa
        ch <- get corHeroi
        if y < 19 then do
                let (ma, _) = setCasa m (x,y) c
                let (map, (nome, d, cor)) = setCasa ma (x,y+1) ch
                corCasa $= (nome, d, cor)
                mapa $= map
                heroi $= ((x,y+1), vida - d)
        else 
            putStr ""
        ((x2,y2), vida2) <- get heroi
        if vida <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then 
                venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

