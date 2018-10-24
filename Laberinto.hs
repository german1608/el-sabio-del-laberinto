{-|
Module      : Laberinto
Description : Tipos de datos y funciones para modificar el laboratorio
Copyright   : (c) German Robayo, 2018
                  Gabriel Gutierrez, 2018
Maintainer  : 14-10924@usb.ve, 13-10625@usb.ve

Contiene los tipos de datos y las funciones que se van a usar
para manipular estos.
-}
module Laberinto (Laberinto, main) where

-- | El tipo de dato Laberinto consiste de una Trifurcacion y un Maybe Tesoro
data Laberinto = Laberinto Trifurcacion (Maybe Tesoro)

data Trifurcacion = Trifurcacion {
    izquierda :: Maybe Laberinto,
    derecha :: Maybe Laberinto,
    recto :: Maybe Laberinto
}

data Tesoro = Tesoro {
    descripcion :: String,
    laberinto :: Maybe Laberinto
}

-- | Main
main = do
    putStrLn "hola mano qlq"
