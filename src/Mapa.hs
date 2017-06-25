module Mapa where
    import Graphics.Rendering.OpenGL hiding (($=))
    import Graphics.UI.GLUT
    import Data.IORef
    -- import Graphics.UI.GLUT.Callbacks.Window
    -- import System.Exit
    -- import Control.Applicative
    import System.IO.Unsafe
    import System.Random
    import Environment

    
    
    
    pinta = True



    -- cores :: [Cor]
    -- cores = [
    --     cor 0 0 0,
    --     cor 1 0 0,
    --     cor 0 1 0,
    --     cor 0 0 1,
    --     cor 1 1 1
    --     ] 


    -- Desenha uma linha do mapa
    linha :: Vertice -> Vertice -> Vertice -> Vertice -> [Objeto] -> IO ()
    linha v1 v2 v3 v4 [cor] = desenhaQuadrado cor v1 v2 v3 v4
    linha v1 v2 v3 v4 (cor:ls) = do 
        desenhaQuadrado cor v1 v2 v3 v4
        linha (mais v1 0.1) (mais v2 0.1) (mais v3 0.1) (mais v4 0.1) ls
            where mais = (\(vx,vy) x -> (vx + x, vy))

    -- Chama a função para desenhar cada linha do mapa
    desenhaMapa :: Vertice -> Vertice -> Vertice -> Vertice -> [[Objeto]] -> IO ()
    desenhaMapa v1 v2 v3 v4 [cor] = linha v1 v2 v3 v4 cor
    desenhaMapa v1 v2 v3 v4 (cor:ls) = do 
        linha v1 v2 v3 v4 cor
        desenhaMapa (menos v1 0.1) (menos v2 0.1) (menos v3 0.1) (menos v4 0.1) ls
            where menos = (\(vx,vy) y -> (vx, vy - y))
    
    -- getCores:: [[Int]] -> [[Cor]]
    -- getCores [] = []
    -- getCores (linha:ls) = [ cores!!c | c <- linha ] : getCores ls

    
    -- Gerar uma lista de numeros entre 0 e 4
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


    -- Desenha o mapa inicial do jogo
    mapa :: IORef [[Objeto]] -> IO ()
    mapa atualizar = do
        cd <- get atualizar
        -- putStrLn (a)
        -- cd <- randomMapa 20 20
        -- if a == 1 then 
        --     do 
        --         atualizar $= False
        --         putStrLn "teste"
        -- else 
        --     do
        --         putStrLn "nop"
        --         return ()
        -- putStrLn "depois"
        clear [ColorBuffer]
        renderPrimitive Quads $ do
            desenhaMapa (vertice (-1) 1) (vertice (-1) 0.9) (vertice (-0.9) 0.9) (vertice (-0.9) 1) cd
        flush

    -- Utilizado para chamar color3f em vez de chamar color $ Color3 r g b
    color3f :: Cor -> IO ()
    color3f (r, g, b) = color $ Color3 r g (b :: GLfloat)
    
    -- Utilizado para chamar vertex2f em vez de chamar vertex $ Vertex2 x y
    vertex2f :: Vertice -> IO ()
    vertex2f (x, y) = vertex $ Vertex2 x y


    -- desenhaQuadradoAux :: Cor -> Vertice -> Vertice -> Vertice -> Vertice -> IO ()
    -- desenhaQuadradoAux cor v1 v2 v3 v4 = do
    --         color3f cor
    --         vertex2f v1
    --         vertex2f v2
    --         vertex2f v3
    --         vertex2f v4

    -- Desenha apenas um quadrado
    desenhaQuadrado :: Objeto -> Vertice -> Vertice -> Vertice -> Vertice -> IO ()
    desenhaQuadrado (_, _, cor) v1 v2 v3 v4 = do
            color3f cor
            vertex2f v1
            vertex2f v2
            vertex2f v3
            vertex2f v4
           


    




    -- Gerar lista de cores (Provisorio)
    --corLinha :: Int -> [Cor]
    --corLinha 1 = cor 1 0 0 : []
    --corLinha q = cor 0 0 1 : corLinha (q-1)

    -- corMapa :: Int -> Int -> [[Cor]]
    -- corMapa 1 l = corLinha l : []
    -- corMapa q l = corLinha l : corMapa (q-1) l

