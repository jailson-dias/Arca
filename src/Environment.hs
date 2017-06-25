module Environment where

import Graphics.Rendering.OpenGL hiding (($=))
import Graphics.UI.GLUT
import Control.Applicative
import Data.IORef
import System.Exit
import Graphics.UI.GLUT.Callbacks.Window
import Control.Concurrent
import Control.Concurrent.MVar

import System.Random
import System.IO.Unsafe

type Cor = (GLfloat, GLfloat, GLfloat)

type Vertice = (GLfloat, GLfloat)

type Damage = Int
type Life = Int

type Objeto = (String, Damage, Cor)

vertice :: GLfloat -> GLfloat -> Vertice
vertice x y = (x, y)

cor :: GLfloat -> GLfloat -> GLfloat -> Cor
cor r g b = (r, g, b)


objetos :: [Objeto]
objetos = [
    ("Ground", 0, (0.625,0.269, 0.07)),
    ("ThornTrap", 15, (0.542, 0, 0)),
    ("ArrowTrap", 25, (0,1,0)),
    ("FireTrap", 40, (0,0,1)),
    ("Hole", 60, (1,1,0))
	]

getObjetos:: [[Int]] -> [[Objeto]]
getObjetos [] = []
getObjetos (linha:ls) = [objetos!!c | c <- linha ] : getObjetos ls