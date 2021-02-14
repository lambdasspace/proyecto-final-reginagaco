module Main where

import Control.Monad
import Tablero

main :: IO ()
main = do
  putStrLn "Bienvenid@ a Othello"
  putStrLn "Para comenzar oprime 1"
  empieza <- getLine
  let s = get.empieza
  if(s /= 1 )
    then error "Entrada inválida"
