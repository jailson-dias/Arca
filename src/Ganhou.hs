module Ganhou where
    
    import Data.IORef
    import Graphics.Rendering.OpenGL hiding (($=))
    import Graphics.UI.GLUT
    import GameOver

    ganhou::IO ()
    ganhou = do
        clear [ColorBuffer]
        renderPrimitive Quads $ do
            desenhaMapa ((-1)::GLfloat, 1::GLfloat) ((-0.9)::GLfloat, 1::GLfloat) ((-0.9)::GLfloat, 0.9::GLfloat) ((-1)::GLfloat, 0.9::GLfloat) ganha
        flush

    ganha = [
        linhaBranca,
        linhaBranca,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaBranca,
        linhaBranca,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaPreta,
        linhaBranca,
        linhaBranca,
        linhaBranca]

    linhaPreta = [
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat),
            (0::GLfloat,0::GLfloat,0::GLfloat)]