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
type Pocao = (Int)

main :: IO ()
main = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered]
    initialWindowSize $= Size 700 600
    createWindow "Arca v0.1" -- Titulo da janela
    heroi <- newIORef ((19::Int, 19::Int), 300::Int)
    corHeroi <- newIORef (1::GLfloat,1::GLfloat,1::GLfloat)
    corCasa <- newIORef ("",5::Int,(1::GLfloat,0.5::GLfloat,0::GLfloat))

    pocao <- newIORef (3::Int)
    venceu <- newIORef (1::Int)

    putStrLn "Tutorial"
    putStrLn "O quadrado preto é o tesouro"
    putStrLn "O quadrado branco é o herói"
    putStrLn "Os quadrados marrom não tiram vida"
    putStrLn "Os quadrados vermelho escuro tiram 15 de vida"
    putStrLn "Os quadrados verdes tiram 25 de vida"
    putStrLn "Os quadrados azuis tiram 40 de vida"
    putStrLn "Os quadrados amarelos tiram 60 de vida"

    nivelJogo <- newIORef ('1'::Char)
    configuracaoNivel <- newIORef (0::Int, 0::Int, 0::Int, 0::Int, 0::Int)
    putStrLn "Escolha o nivel de dificuldade com as teclas de 1 a 3"
    escolhaNivel nivelJogo configuracaoNivel

    mapa <- newIORef []
    setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
    displayCallback $= (display mapa heroi venceu)
    attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi pocao venceu nivelJogo configuracaoNivel

    mainLoop

setMapa :: IORef [[Objeto]] -> IORef Heroi -> IORef CorMain -> IORef Objeto -> IORef ConfiguracaoNivel -> IORef Int -> IORef Int -> IO ()
setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao = do
    venceu $= 1
    pocao $= 5
    heroi $= ((19, 19), 300)
    cd <- randomMapa 20 20 configuracaoNivel
    (h, _) <- get heroi
    c <- get corHeroi
    let (m, _) = setCasa (getObjetos cd) (0,0) (0::GLfloat,0::GLfloat,0::GLfloat)
    let (ma, co) = setCasa m h c
    mapa $= ma
    corCasa $= co

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
    swapBuffers

myKeyboardMouseCallback mapa heroi corCasa corHeroi pocao venceu nivelJogo configuracaoNivel key keyState modifiers position =
  case (key, keyState) of
    (Char 'p', Up) -> utilizarPocao pocao heroi
    (Char '1', Up) -> do 
        nivelJogo $= '1'
        escolhaNivel nivelJogo configuracaoNivel
        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
        postRedisplay Nothing
    (Char '2', Up) -> do 
        nivelJogo $= '2'
        escolhaNivel nivelJogo configuracaoNivel
        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
        postRedisplay Nothing
    (Char '3', Up) -> do 
        nivelJogo $= '3'
        escolhaNivel nivelJogo configuracaoNivel
        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
        postRedisplay Nothing
    (SpecialKey KeyRight, Up) -> keyRight mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo
    (SpecialKey KeyLeft, Up) -> keyLeft mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo
    (SpecialKey KeyUp, Up) -> keyUp mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo
    (SpecialKey KeyDown, Up) -> keyDown mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo

    _ -> return () -- ignore other buttons

attachMyKeyboardMouseCallback mapa heroi corCasa corHeroi pocao venceu nivelJogo configuracaoNivel = keyboardMouseCallback $= Just (myKeyboardMouseCallback mapa heroi corCasa corHeroi pocao venceu nivelJogo configuracaoNivel)

utilizarPocao :: IORef Pocao -> IORef Heroi -> IO()
utilizarPocao pocao heroi
 = do
     p <- get pocao
     ((a,b), vida) <- get heroi
     if (p > 0) then
         do
             pocao $= (p - 1)
             putStrLn("Mais 20% de vida!")
             putStrLn("Sua vida agora é: " ++ show(vida + 60))
             heroi $= ((a,b), vida + 60)
             return()
     else
         do
             putStrLn("Não é possível utilizar mais poção!")
             return()

--usuario escolhe o nivel do jogo [1,2,3]
escolhaNivel :: IORef Nivel -> IORef ConfiguracaoNivel -> IO()
escolhaNivel nivelJogo configuracaoNivel = do
     nivel <- get nivelJogo
     case nivel of
         '1' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (40, 65, 89, 97, 100) --porcentagem somada uma a uma até 100%
             return()
         '2' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (30, 58, 82, 90, 100)
             return()
         '3' -> do
             nivelJogo $= nivel
             putStrLn("Nível escolhido: " ++ show nivel)
             configuracaoNivel $= (15, 35, 60, 80, 100)
             return()
         x | x >= ' ' -> do
             escolhaNivel nivelJogo configuracaoNivel
         _ -> do
             putStrLn("Nível escolhido inválido.")
             escolhaNivel nivelJogo configuracaoNivel


-- Alterar a cor de uma casa no mapa
setCasa :: [[Objeto]] -> Posicao -> CorMain -> ([[Objeto]], Objeto)
setCasa mapa (x, y) cor = (take x mapa ++ [take y (mapa!!x) ++ [(\(nome, dano, cor) nova -> (nome,dano, nova)) (mapa!!x!!y) cor] ++ drop (y+1) (mapa!!x)] ++ drop (x+1) mapa, mapa!!x!!y)


keyUp :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IORef ConfiguracaoNivel -> IORef Int -> IORef Char -> IO ()
keyUp mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo = do
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
        if vida2 <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then do
                nj <- get nivelJogo
                case nj of
                    '1' -> do 
                        nivelJogo $= '2'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    '2' -> do
                        nivelJogo $= '3'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    _ -> venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

-- Movimento para baixo
keyDown :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IORef ConfiguracaoNivel -> IORef Int -> IORef Char  -> IO ()
keyDown mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo = do
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
        if vida2 <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then do
                nj <- get nivelJogo
                case nj of
                    '1' -> do 
                        nivelJogo $= '2'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    '2' -> do
                        nivelJogo $= '3'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    _ -> venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else 
        putStr ""

-- Movimento para esquerda
keyLeft :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IORef ConfiguracaoNivel -> IORef Int -> IORef Char  -> IO ()
keyLeft mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo = do
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
        if vida2 <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then do
                nj <- get nivelJogo
                case nj of
                    '1' -> do 
                        nivelJogo $= '2'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    '2' -> do
                        nivelJogo $= '3'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    _ -> venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

-- Movimento para direita
keyRight :: IORef [[Objeto]] -> IORef Heroi -> IORef Objeto -> IORef CorMain -> IORef Int -> IORef ConfiguracaoNivel -> IORef Int -> IORef Char  -> IO ()
keyRight mapa heroi corCasa corHeroi venceu configuracaoNivel pocao nivelJogo = do
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
        if vida2 <= 0 then 
            venceu $= 0
        else 
            if x2 == 0 && y2 == 0 then do
                nj <- get nivelJogo
                case nj of
                    '1' -> do 
                        nivelJogo $= '2'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    '2' -> do
                        nivelJogo $= '3'
                        escolhaNivel nivelJogo configuracaoNivel
                        setMapa mapa heroi corHeroi corCasa configuracaoNivel venceu pocao
                    _ -> venceu $= 2
            else
                putStr ""
        putStrLn ("x: " ++ show x2 ++ ", y: " ++ show y2 ++ ", vida: " ++ show vida2)
        postRedisplay Nothing
    else
        putStr ""

