module Tablero where

import Data.Map (Map)
import Data.List (List)


{- Definicion de un tipo de dato para la creación de un tablero para el juego de
Othello -}
data Color = Blanco| Negro| Vacia deriving (Eq, Show) -- Estados de las casillas
type Posicion = (Int, Int) --Coordenadas en el tablero de la ficha
type Tablero = Map.Map Posicion Color -- Composición del tablero

-- Creación de lista de los posibles movimientos en el tablero
movimientos :: [Posicion]
movimientos = [(x,y)| x <- [0..7], y <- [0..7]]

-- Tablero vacío
vacio :: Tablero
vacio = Map.fromList(List.zip movimientos (repeat Vacia))

-- Tablero Inicial: El juego siempre se inicia con 4 piezas, dos blancas y dos
-- negras en diagonal en el centro del tablero.
inicial :: Tablero
inicial = Map.union(fromList (List.zip [(3,3), (3,4), (4,4), (4,3)]
 [Blanco, Negro, Blanco, Negro])) vacia

 -- Lista de los primeros posibles movimientos (las casillas al rededor de las
 -- fichas centrales)
 primerosMov :: [Posicion]
 primerosMov = [(2,5),(3,5),(4,5),(5,5),(2,4),(5,4),(2,3),(5,3),(2,2),(3,2),(4,2),(5,2)]

-- Regresa el color contrario al del jugador
contrario :: Color -> Color
contrario Negro = Blanco
contrario Blanco = Negro
contrario Vacia = Vacia

{- Comportamiento -}

-- Revisa todas las direcciones y devuelve una lista de movimientos válidos
direcciones :: Posicion -> String -> [Posicion]
direcciones (x,y) direccion
  |direccion == "Ar" = [(x, y+h)]    | h <- [1..7], y+h <=7]
  |direccion == "D" = [(x+h, y)]    | h <- [1..7], x+h <=7]
  |direccion == "Ab" = [(x, y-h)]    | h <- [1..7], y-h >=0]
  |direccion == "I" = [(x-h, y)]    | h <- [1..7], x-h >=0]
  |direccion == "ArD" = [(x+h, y+h)] | h <- [1..7], x+h <=7, y+h <=7]
  |direccion == "AbD" = [(x+h, y-h)  | h <- [1..7], x+h <=7, y-h >=0]
  |direccion == "AbI" = [(x-h, y-h)  | h <- [1..7], x-h >=0, y-h >=0]
  |otherwise = [(x-h, y+h) | h <- [1..7], x-h >=0, y+h <=7]


-- valido : Regresa True si el movimiento es válido
valido :: Tablero -> Color -> [Posicion] -> Bool
valido t c p = (Map.!) t p == Vacia && List.length() > 0

--Validos : Regresa una lista de movimientos válidos
validos :: Tablero -> Color -> [Posicion] -> [Posicion]
validos t c lp = List.filter (\p -> valido t c p) lp

-- colores : Regresa una lista de colores de la posicion dada
colores :: Tablero -> [Posicion] -> [Color]
colores t = List.map(t Map.!)

-- capturada: Regresa una lista de las fichas capturadas que deben cambiar de
-- color
capturada :: Tablero -> Posicion -> Color -> [Posicion]
capturada = List.concat[]

-- Capturadas : Regresa una lista de piezas que tienen que cambiar de colors
capturadas :: Tablero -> Color -> [Posicion] -> [Posicion]

{--}
-- Piezas : Regresa el número de fichas que tiene un jugador
piezas :: Tablero -> Color -> Int
