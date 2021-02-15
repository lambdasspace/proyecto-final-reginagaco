module Main where

import Control.Monad
import Tablero

main :: IO ()
main = do
  putStrLn "Bienvenid@ a Othello"
  putStrLn "Para comenzar oprime 1"
  empieza <- getLine
  let s = read empieza
  if(s /= 1 )
    then error "Entrada inválida"
    else
      do
        putStrLn "Ingresa el nivel de dificultad:"
        pr <-  agente
        juego pr inicial Blanco primerosMov
