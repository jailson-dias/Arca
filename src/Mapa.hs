module Mapa where
    import Graphics.Rendering.OpenGL hiding (($=))
    import Graphics.UI.GLUT
    import System.IO.Unsafe
    import System.Random

    type Cor = (GLfloat, GLfloat, GLfloat)
    type Vertice = (GLfloat, GLfloat)
    
    vertice :: GLfloat -> GLfloat -> Vertice
    vertice x y = (x, y)

    cor :: GLfloat -> GLfloat -> GLfloat -> Cor
    cor r g b = (r, g, b)

    cores :: [Cor]
    cores = [
        cor 0 0 0,
        cor 1 0 0,
        cor 0 1 0,
        cor 0 0 1,
        cor 1 1 1
        ] 

    for :: Vertice -> Vertice -> Vertice -> Vertice -> [Cor] -> IO ()
    for v1 v2 v3 v4 [cor] = cube cor v1 v2 v3 v4
    for v1 v2 v3 v4 (cor:ls) = do 
        cube cor v1 v2 v3 v4
        for (mais v1 0.1) (mais v2 0.1) (mais v3 0.1) (mais v4 0.1) ls
            where mais = (\(vx,vy) x -> (vx + x, vy))

    desenhaLinha :: Vertice -> Vertice -> Vertice -> Vertice -> Int -> IO ()
    desenhaLinha v1 v2 v3 v4 1 = for v1 v2 v3 v4 [
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 1),
                (cor 1 1 0),
                (cor 1 1 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 1),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0)
                ]
    desenhaLinha v1 v2 v3 v4 a = do 
        for v1 v2 v3 v4 [
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 1),
                (cor 1 1 0),
                (cor 1 1 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 1),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0),
                (cor 1 0 0)
                ]
        desenhaLinha (menos v1 0.1) (menos v2 0.1) (menos v3 0.1) (menos v4 0.1) (a-1)
            where menos = (\(vx,vy) y -> (vx, vy - y))

    gerarCor :: Int -> [Int]
    gerarCor 0 = []
    gerarCor q = do 
        -- cd <- randomRIO (0::Int, 4::Int)
        let cd = unsafePerformIO (randomRIO (0::Int, 4::Int))
        -- putStrLn (show cd)
        cd : gerarCor (q-1)

    mapa :: IO ()
    mapa = do
        cd <- randomRIO (1::Int, 10::Int)
        putStrLn (show cd)
        clear [ColorBuffer]
        renderPrimitive Quads $ do
            desenhaLinha (vertice (-1) 1) (vertice (-1) 0.9) (vertice (-0.9) 0.9) (vertice (-0.9) 1) 20
        flush

    color3f :: Cor -> IO ()
    color3f (r, g, b) = color $ Color3 r g (b :: GLfloat)
    
    vertex2f :: Vertice -> IO ()
    vertex2f (x, y) = vertex $ Vertex2 x y

    cube :: Cor -> Vertice -> Vertice -> Vertice -> Vertice -> IO ()
    cube cor v1 v2 v3 v4 = do
            color3f cor
            vertex2f v1
            vertex2f v2
            vertex2f v3
            vertex2f v4