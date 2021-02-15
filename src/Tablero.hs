module Tablero where

import Data.Map as Map
import Data.List as List


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
inicial = Map.union(fromList (List.zip [(3,3), (3,4), (4,4), (4,3)][Blanco, Negro, Blanco, Negro])) vacio

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
  |direccion == "Ar"  = [(x, y+h)   | h <- [1..7], y+h <=7]
  |direccion == "D"   = [(x+h, y)   | h <- [1..7], x+h <=7]
  |direccion == "Ab"  = [(x, y-h)   | h <- [1..7], y-h >=0]
  |direccion == "I"   = [(x-h, y)   | h <- [1..7], x-h >=0]
  |direccion == "ArD" = [(x+h, y+h) | h <- [1..7], x+h <=7, y+h <=7]
  |direccion == "AbD" = [(x+h, y-h)  | h <- [1..7], x+h <=7, y-h >=0]
  |direccion == "AbI" = [(x-h, y-h)  | h <- [1..7], x-h >=0, y-h >=0]
  |otherwise = [(x-h, y+h) | h <- [1..7], x-h >=0, y+h <=7]


-- valido : Regresa True si el movimiento es válido
valido :: Tablero -> Color -> Posicion -> Bool
valido t c p = (Map.!) t p == Vacia && List.length(capturada t p c) > 0

--Validos : Regresa una lista de movimientos válidos
validos :: Tablero -> Color -> [Posicion] -> [Posicion]
validos t c lp = List.filter (\p -> valido t c p) lp

-- colores : Regresa una lista de colores de la posicion dada
colores :: Tablero -> [Posicion] -> [Color]
colores t = List.map(t Map.!)

-- capturada: Regresa una lista de las fichas capturadas que deben cambiar de
-- color
capturada :: Tablero -> Posicion -> Color -> [Posicion]
capturada t p c = List.concat[capturadas t c l | l <- List.map(direcciones p) ["Ar", "D", "Ab", "I", "ArD", "AbD", "AbI"]]

-- Capturadas : Regresa una lista de piezas que tienen que cambiar de colors
capturadas :: Tablero -> Color -> [Posicion] -> [Posicion]
capturadas _ _ [] = []
capturadas _ _ (_:[]) = []
capturadas t c lp
 | z /= [] && fst(head z) == c = List.map snd(List.takeWhile(\x -> fst x == contrario c)(zip(colores t lp) lp))
 | otherwise = []
 where
   z = List.dropWhile(\x -> fst x == contrario c) (zip(colores t lp)lp)

{--}
-- Fichas : Regresa el número de fichas que tiene un jugador
fichas :: Tablero -> Color -> Int
fichas t c = Map.foldr(\valor acc -> if valor == c then 1 + acc else acc) 0 t

-- Campeon : Regresa el color del jugador con más fichas en el tablero
campeon:: Tablero -> Color
campeon t = if fichasN > fichasB then Negro
         else if fichasB == fichasN then Vacia
          else Blanco
     where
       fichasN = fichas t Negro
       fichasB = fichas t Blanco

-- pintaMov : Pinta el movimiento del jugador en el teclado
pintaMov :: Posicion -> Tablero -> Color -> Tablero
pintaMov p t c =
  let lp = capturada t p c
  in Map.union(fromList[(p,c)])
    (Map.union(fromList $ List.zip lp (repeat $ c)) t)

-- getMovimiento : Obtiene el movimiento del jugador leyendolo de la terminal
getMovimiento :: Tablero -> Color -> IO Posicion
getMovimiento tablero color = do
  l <- getLine
  return $ read l

{- Definición e inicialización del agente-}

-- Agente :
agente :: IO Int
agente = do
  putStrLn "Elige el nivel de dificultad"
  putStrLn "Puedes elegir entre el nivel 1,2,3,4 o 5"
  putStrLn "Ingrese el numero de la dificultad que desea:"
  profundidad <- getLine
  let pr = read profundidad
  if (pr /= 1 && pr /= 2 && pr /= 3 && pr /= 4 && pr /= 5)
    then error "Entrada invalida"
    else return $ read profundidad

--movAgente : Regresa el mejor movimiento para el agente
-- pr = profundidad p = posición
movAgente :: Tablero -> Color -> Int -> [Posicion] -> Posicion
movAgente t c pr lp =
  (\(p, _) -> p) (List.maximumBy
  (\(_,valorTablero1) (_,valorTablero2) -> compare valorTablero1 valorTablero2)
  (List.map(\pos -> (pos, valorTablero (pintaMov pos t c) c c pr lp))(validos t c lp)))

--ValorTablero : Regresa el valor del tablero
-- Recibe: un tablero, el color del jugador, la lista de movimientos y la profundidad
valorTablero :: Tablero -> Color -> Color -> Int -> [Posicion] -> Int
valorTablero t cj c pr lp =
  let sColor = contrario c
      sColorMovVal  = validos t sColor lp
      sColorValorTablero = maximum (
        List.map
          (\p -> valorTablero (pintaMov p t c) cj sColor (pr - 1) (actTablero t p lp))
          sColorMovVal)
      thisValorTablero = length $ validos t c lp
  in if (List.null sColorMovVal)
    then if (campeon board) == cj then 10000 else -10000
    else
      if pr > 0
        then if sColor == cj then sColorValorTablero else - sColorValorTablero
        else if sColor == cj then thisValorTablero else - thisValorTablero

--ValorFila: Regresa el valor de la fila
valorFila :: Tablero -> Color -> Int
valorFila t c =
  if c == Negro
    then puntajeN - puntajeB
    else puntajeB - puntajeN
    where
      puntajeN = (fichas t Negro)
      puntajeB = (fichas t Blanco)

--ActTablero: Actualiza el tablero
actTablero:: Tablero -> Posicion -> [Posicion]
actTablero _ _ [] = []
actTablero t p lp =
  do
    eliminaElem  (x,y) lp3
    where x = fst p
          y = snd p
          arriba = if (y+1 < 8) then
            if (head (colores t [(x,y+1)])) == Vacia then (x,y+1) else (-1,-1)
            else (-1,-1)
          abajo = if (y-1 >= 0) then
            if (head (colores t [(x,y-1)])) == Vacia then (x,y-1) else (-1,-1)
            else (-1,-1)
          izquierda = if (x-1 >= 0) then
            if (head (colores t [(x-1,y)])) == Vacia then (x-1,y) else (-1,-1)
            else (-1,-1)
          derecha = if (x+1 < 8) then
            if (head (colores t [(x+1,y)])) == Vacia then (x+1,y) else (-1,-1)
            else (-1,-1)
          arribaIzquierda = if (y+1 < 8 && x-1 >= 0) then
            if (head (colores t [(x-1,y+1)])) == Vacia then (x,y+1) else (-1,-1)
            else (-1,-1)
          arribaDerecha = if (y+1 < 8 && x+1 < 8) then
              if (head (colores t [(x+1,y+1)])) == Vacia then (x,y+1) else (-1,-1)
              else (-1,-1)
          abajoIzquierda = if (y-1 >= 0 && x-1 >= 0) then
              if (head (colores t [(x-1,y-1)])) == Vacia then (x-1,y-1) else (-1,-1)
              else (-1,-1)
          abajoDerecha = if (y-1 >= 0 && x+1 < 8) then
            if (head (colores t [(x+1,y-1)])) == Vacia then (x+1,y-1) else (-1,-1)
            else (-1,-1)
          lp1 = lp ++ [arriba,abajo,izquierda,derecha,arribaIzquierda,arribaDerecha,abajoIzquierda,abajoDerecha]
          lp2 = eliminaDuplicados lp1
          lp3 = limpiaLista (-1,-1) lp2

--EliminaDuplicados : Elimina los elementos duplicados y devuelve una lista con
--solo una instancia de ellos
eliminaDuplicados :: Eq t => [t] -> [t]
eliminaDuplicados [] = []
eliminaDuplicados (e1:r)
  | elem e1 r = eliminaDuplicados r
  | otherwise = e1 : eliminaDuplicados r

--LimpiaLista : Elimina las apariciones de un elemento en una lista
limpiaLista :: Eq t => t -> [t] -> [t]
limpiaLista _ [] = []
limpiaLista x (y:ys)
  | x == y = limpiaLista x ys
  | otherwise = y : limpiaLista x ys

--EliminaELem : Elimina la primera aparición de un elemento en la lista
eliminaElem :: Eq t => t -> [t] -> [t]
eliminaElem _ [] = []
eliminaElem x (y:ys)
  | x == y = ys
  | otherwise = y : eliminaElem x ys


{-Impresion de los tableros: Aunque tradicionalmente se representan a los
jugadores con circulos blancos o negros debido a la falta de colores el jugador
blanco será representado por el símbolo "O" y el jugador con fichas negras por
el símbolo "X".-}

--ImprimeFicha:: Imprime el símbolo del jugador
imprimeFicha :: Color -> String
imprimeFicha c
  | c == White = "O"
  | c == Black = "X"
  | otherwise = " "

--ImprimeFila : Imprime una fila del tablero
--t : tablero f: fila
imprimeFila :: Tablero -> Int -> String
imprimeFila t f = show f ++ " | " ++
  (intercalate " | "
    (List.map(\pos -> print_symbol (board ! pos))
    ([(x, f) | x <- [0..7]]))) ++" | " ++ show f

--ImprimeTablero
imprimeTablero :: Tablero -> String
imprimeTablero =
  "\n    0   1   2   3   4   5   6   7 \n -----------------------------------\n" ++
  (intercalate
  "\n -----------------------------------\n"
  (List.map (imprimeFila board) $ [0..7])) ++ "\n -----------------------------------\n    0   1   2   3   4   5   6   7 "

{-Juego-}

--Juego:
-- pr : profundidad t:tablero c:color l:lista de posiciones
juego :: Int -> Tablero -> Color -> [Posicion] -> IO()
juego pr t c lp = do
  let l = validos t c lp
  if (List.null l)
    then
      do
      putStrLn $ imprimeTablero t
      putStrLn ("El jugador con fichas negras tiene" ++ (show(fichas t Negro)) ++ "puntos")
      putStrLn ("El jugador con fichas blancas tiene"++ (show(fichas t Blanco))++ "puntos")
      if campeon == Vacio then putStrLn "¡Empate! o-o"
        else if campeon == Negro
          then do
            putStrLn "¡El jugador con fichas negras ganó!"
          else putStrLn "¡El jugador con fichas blancas ganó!"
    else
      do
      putStrLn $ imprimeTablero t
      if color == Blanco
        then
          do
          putStrLn "Haz tu movimiento jugador blanco"
          putStrLn ("Quieres una sugerencia?:" ++ show(movAgente c pr l))
          putStrLn $ List.concat $ List.map show (validos t c l)
          mb <- movBlanco
          if valido t Blanco mb
            then
              juego pr (pintaMov mb t Blanco) (contrario c) (actTablero t mb lp)
            else
              do
              putStrLn "Movimiento Invalido: No olvides las reglas"
              juego pr t c lp
    else
      do
      putStrLn "El agente esta buscando un movimiento"
      let
        mov = (movAgente t c pr lp)
        l1 = actTablero t mov lp
      juego pr (pintaMov mov t Negro) (contrario c) l1

  where
    ganador = campeon t
    mb = getMovimiento t Blanco
    mn = getMovimiento t Negro
