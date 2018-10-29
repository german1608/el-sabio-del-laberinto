{-|
Module      : Laberinto
Description : Tipos de datos y funciones para modificar el laboratorio
Copyright   : (c) German Robayo, 2018
                  Gabriel Gutierrez, 2018
Maintainer  : 14-10924@usb.ve, 13-10625@usb.ve

Contiene los tipos de datos y las funciones que se van a usar
para manipular estos.
-}
module Laberinto where

-- | El tipo de dato Laberinto consiste de una Trifurcacion y un Maybe Tesoro
type Laberinto = Either Trifurcacion Tesoro

-- | El tipo de dato Trifurcacion nos permite saber hacia donde podemos ir
data Trifurcacion = Trifurcacion {
    izquierda :: Maybe Laberinto,
    derecha :: Maybe Laberinto,
    recto :: Maybe Laberinto
} deriving Show

-- | El tipo de dato Tesoro nos permite saber que es el tesoro y que mas puedo encontrar.
data Tesoro = Tesoro {
    descripcion :: String,
    laberinto :: Maybe Laberinto
} deriving Show

-- | Direccion hacia donde dirigirme recorriendo el laberinto
data Direccion = Izquierda | Derecha | Recto
    deriving Show

-- Funciones de Construccion
-- | Funcion que crea un camino sin salida
caminoSinSalida :: Trifurcacion
caminoSinSalida = Trifurcacion {
    izquierda = Nothing,
    derecha = Nothing,
    recto = Nothing
}

-- | Laberinto vacio
laberintoVacio :: Laberinto
laberintoVacio = Left caminoSinSalida

-- | Funcion que crea un Tesoro dada la descripcion y el laberinto
crearTesoro :: String -> Laberinto -> Tesoro
crearTesoro descripcion laberinto = Tesoro {
    descripcion = descripcion,
    laberinto = Just laberinto
}

-- | Funcion que altera el laberinto hacia donde se va a ir en cierto lado de
-- una trifurcacion
alterarTrifurcacion :: Trifurcacion -> Laberinto -> Direccion -> Trifurcacion
alterarTrifurcacion trifur lab Izquierda = Trifurcacion {
    izquierda = Just lab,
    derecha = derecha trifur,
    recto = recto trifur
}
alterarTrifurcacion trifur lab Derecha = Trifurcacion {
    izquierda = izquierda trifur,
    derecha = Just lab,
    recto = recto trifur
}
alterarTrifurcacion trifur lab Recto = Trifurcacion {
    izquierda = izquierda trifur,
    recto = Just lab,
    derecha = derecha trifur
}

-- Funciones de Acceso

-- | Funcion que permite acceder a un atributo de una trifurcacion
-- dado el lado que se desea explorar
obtenerLabDeTrifur :: Trifurcacion -> Direccion -> Maybe Laberinto
obtenerLabDeTrifur t Izquierda = izquierda t
obtenerLabDeTrifur t Derecha = derecha t
obtenerLabDeTrifur t Recto = recto t

-- | Funcion que dado un tesoro y una direccion, retorna lo que se encontraria
-- si sigue esa direccion.
obtenerLabDeTesoro :: Tesoro -> Direccion -> Maybe Laberinto
obtenerLabDeTesoro t Recto = laberinto t
obtenerLabDeTesoro _ _ = Nothing

-- | Funcion que dado un laberinto y una direccion, retorna un Maybe laberinto
-- indicando que se descubrira
obtenerLaberintoPorDir :: Laberinto -> Direccion -> Maybe Laberinto
obtenerLaberintoPorDir lab dir = case lab of
    -- Cuando es una trifurcacion, puedo retornar lo que diga la direccion
    Left trifur -> obtenerLabDeTrifur trifur dir
    Right tesoro -> obtenerLabDeTesoro tesoro dir

-- Funcion que dado un laberinto me da el laberinto (o no) que conseguira
-- si se dirige a la izquierda
izquierdaLab :: Laberinto -> Maybe Laberinto
izquierdaLab = flip obtenerLaberintoPorDir $ Izquierda

-- Funcion que dado un laberinto me da el laberinto (o no) que conseguira
-- si se dirige a la derecha
derechaLab :: Laberinto -> Maybe Laberinto
derechaLab = flip obtenerLaberintoPorDir $ Derecha

-- Funcion que dado un laberinto me da el laberinto (o no) que conseguira
-- si sigue derecho
rectoLab :: Laberinto -> Maybe Laberinto
rectoLab = flip obtenerLaberintoPorDir $ Recto
