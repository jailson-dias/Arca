module Mapa where
    import Graphics.Rendering.OpenGL hiding (($=))
    import Graphics.UI.GLUT
    import Data.IORef
    import System.IO.Unsafe
    import System.Random
    import Environment    
    
    pinta = True

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
    
 
    -- Desenha o mapa inicial do jogo
    mapa :: IORef [[Objeto]] -> IO ()
    mapa atualizar = do
        cd <- get atualizar
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

    -- Desenha apenas um quadrado
    desenhaQuadrado :: Objeto -> Vertice -> Vertice -> Vertice -> Vertice -> IO ()
    desenhaQuadrado (_, _, cor) v1 v2 v3 v4 = do
            color3f cor
            vertex2f v1
            vertex2f v2
            vertex2f v3
            vertex2f v4