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

-- | Cadena de Direcciones
type Ruta = [Direccion]

-- | Funcion que lee un string y lo transforma a direcciones
parsearRuta :: String -> [Direccion]
parsearRuta = map (\c -> case c of
        '>' -> Derecha
        '<' -> Izquierda
        '^' -> Recto) . filter (\x -> x `elem` "<>^")

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
crearTesoro :: String -> Maybe Laberinto -> Tesoro
crearTesoro descripcion laberinto = Tesoro {
    descripcion = descripcion,
    laberinto = laberinto
}

-- | Funcion que altera el laberinto hacia donde se va a ir en cierto lado de
-- una trifurcacion
alterarTrifurcacion :: Trifurcacion -> Maybe Laberinto -> Direccion -> Trifurcacion
alterarTrifurcacion trifur lab Izquierda = Trifurcacion {
    izquierda = lab,
    derecha = derecha trifur,
    recto = recto trifur
}
alterarTrifurcacion trifur lab Derecha = Trifurcacion {
    izquierda = izquierda trifur,
    derecha = lab,
    recto = recto trifur
}
alterarTrifurcacion trifur lab Recto = Trifurcacion {
    izquierda = izquierda trifur,
    recto = lab,
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

---------------------------
-- Funciones de utilidad --
---------------------------

-- | Funcion que recibe una cadena de direcciones y construye un laberinto a partir
-- de esa ruta
construirLaberintoDeRuta :: Ruta -> Laberinto
construirLaberintoDeRuta [] = Left caminoSinSalida
construirLaberintoDeRuta (x:xs) =
    Left $ alterarTrifurcacion caminoSinSalida (Just $ construirLaberintoDeRuta xs) x


-- | Funcion que recorre el laberinto hasta que encuentre una pared.
-- Cuando se consiga, se modifica el laberinto de modo que se extienda
-- el laberinto con la ruta restante.
abrirRutaPared :: Laberinto -> Ruta -> Laberinto
abrirRutaPared l [] = l
abrirRutaPared l (x:xs) =
    -- Extraemos la trifurcacion
    let Left trif = l
        -- Obtenemos el empate
        labSiguiente = obtenerLaberintoPorDir l x
        laberintoAPegar = case labSiguiente of
            -- Llegue a una pared, me toca construir un nuevo laberinto
            Nothing -> construirLaberintoDeRuta xs
            -- Puedo seguir recorriendo
            Just lab -> abrirRutaPared lab xs
    in Left $ alterarTrifurcacion trif (Just laberintoAPegar) x

-- | Funcion que dada una direccion, un laberinto y una ruta pone en Nothing
-- la direccion en el laberinto luego de recorrer la ruta
derrumbarPared :: Laberinto -> Ruta -> Direccion -> Maybe Laberinto
derrumbarPared l [] d =
    let Left trif = l in Just $ Left $ alterarTrifurcacion trif Nothing d
derrumbarPared l (x:xs) d =
    let labASeguir = obtenerLaberintoPorDir l x
        Left trif = l
        in Just $ case labASeguir of
            Just lab -> Left $ alterarTrifurcacion trif (derrumbarPared lab xs d) x
            Nothing -> Left $ alterarTrifurcacion trif (derrumbarPared l xs d) x

-- | Funcion que dado un laberinto y una ruta determina si al final de la ruta
-- el usuario se encontrara en un tesoro o no.
caigoEnTesoro :: Laberinto -> Ruta -> Bool
caigoEnTesoro (Right tesoro) [] = True
caigoEnTesoro (Left trif) [] = False
caigoEnTesoro l (r:rs) =
    let labASeguir = obtenerLaberintoPorDir l r in
        case labASeguir of
            Just lab -> True && caigoEnTesoro lab rs
            Nothing -> True && caigoEnTesoro l rs

-- | Funcion que toma un laberinto, una ruta y si al final de esta hay un tesoro,
-- se elimina y si habia un camino despues del mismo se reemplaza con este.
-- La precondicion es que la ruta que se siga en el laberinto lleve a un tesoro
-- cuando la ruta este vacia
tomarTesoro :: Laberinto -> Ruta -> Laberinto
-- El caso con la ruta vacia no es de nuestro interes
tomarTesoro _ [] = error "Uso incorrecto de tomarTesoro"
-- Ya recorri toda la ruta y el siguiente step es un tesoro por la
-- precondicion
tomarTesoro l [x] =
    let (Just tesoro) = obtenerLaberintoPorDir l x
        despuesDeTesoro = obtenerLaberintoPorDir tesoro x
    in case l of
        Left trif -> Left $ alterarTrifurcacion trif despuesDeTesoro x
        Right tesoro -> Right $ crearTesoro (descripcion tesoro) despuesDeTesoro

-- Hay que seguir recorriendo
tomarTesoro l@(Right tesoro) (r:rs) =
    let labASeguir = obtenerLaberintoPorDir l r in
        case labASeguir of
            Nothing ->
                tomarTesoro l rs
            Just lab ->
                Right $ crearTesoro (descripcion tesoro) $ Just $ tomarTesoro lab rs
                -- tomarTesoro (Right $ crearTesoro (descripcion tesoro) lab) rs
tomarTesoro l@(Left trif) (r:rs) =
    let labASeguir = obtenerLaberintoPorDir l r in
        case labASeguir of
            Nothing -> tomarTesoro l rs
            Just lab -> Left $ alterarTrifurcacion trif (Just $ tomarTesoro lab rs) r

-- | Funcion que recorre una ruta y anade un mapa justo en entre
-- el laberinto anterior a terminar la ruta y el siguiente (si hay).
-- Como precondicion establezco que debe haber al menos una ruta
ponerTesoro :: Laberinto -> Ruta -> Laberinto
-- Aqui empato el laberinto que se obtiene siguiendo la ruta con el tesoro nuevo
-- y este con el actual
ponerTesoro l [x] =
    let labSiguiente = obtenerLaberintoPorDir l x
        tesoroNuevo = Just $ Right $ crearTesoro "" labSiguiente
        nuevoLaberinto = case l of
            Left trif -> Left $ alterarTrifurcacion trif tesoroNuevo x
            Right tesoro -> Right $ crearTesoro (descripcion tesoro) tesoroNuevo
    in nuevoLaberinto

-- Llamo recursivamente para ir construyendo el laberinto nuevo
ponerTesoro l (x:xs) =
    let labSiguiente = obtenerLaberintoPorDir l x
    in case labSiguiente of
        Nothing -> ponerTesoro l xs
        Just lab -> let nuevoLaberinto = Just $ ponerTesoro lab xs in case lab of
            Left trif -> Left $ alterarTrifurcacion trif nuevoLaberinto x
            Right tesoro -> Right $ crearTesoro (descripcion tesoro) nuevoLaberinto
