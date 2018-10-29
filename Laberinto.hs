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

-- | El tipo de dato Trifurcacion nos permite construir trifurcaciones en el laberinto
-- Las trifurcaciones son las que le dan al laberinto su estructura particular
data Trifurcacion = Trifurcacion {
    izquierda :: Maybe Laberinto, -- ^ Lo descubrible si vamos hacia la izquierda
    derecha :: Maybe Laberinto, -- ^ Lo descubrible si vamos hacia la derecha
    recto :: Maybe Laberinto -- ^ Lo descubrible si seguimos derecho
} deriving (Show, Read)

-- | El tipo de dato Tesoro nos permite saber que es el tesoro y que mas puedo encontrar.
data Tesoro = Tesoro {
    descripcion :: String, -- Descripcion del tesoro en caso de que decidamos reclamarlo
    laberinto :: Maybe Laberinto -- Que conseguimos si seguimos de largo ignorando el tesoro
} deriving (Show, Read)

-- | Tipo de dato cuyo unico uso es evitar usar strings para identificar las direcciones
data Direccion = Izquierda
    | Derecha
    | Recto
    deriving Show

-- | Cadena de Direcciones. Se usa para almacenar las rutas que ingresa el usuario
type Ruta = [Direccion]

{- | Función que lee un string y lo transforma a direcciones.
La función ignora los caracteres no pertenecientes a @"><^"@
y hace el siguiente mapping:

@
> -> Derecha
> -> Izquierda
^ -> Recto
@

Ejemplos:

@
>>> parsearRuta "^^^^"
[Recto,Recto,Recto,Recto]
>>> parsearRuta "<><>"
[Izquierda,Derecha,Izquierda,Derecha]
>>> parsearRuta "letras super random>"
[Derecha]
@

-}
parsearRuta :: String -> [Direccion]
parsearRuta = map (\c -> case c of
        '>' -> Derecha
        '<' -> Izquierda
        '^' -> Recto) . filter (\x -> x `elem` "<>^")

-- Funciones de Construccion

{- | Función que crea un camino sin salida.
Un camino sin salida es una 'Trifurcacion' en la que todos sus lados llevan a @Nothing@.
Básicamente es equivalente a esto:

@
Trifurcacion {
    izquierda = Nothing,
    derecha = Nothing,
    recto = Nothing
}
@
-}
caminoSinSalida :: Trifurcacion
caminoSinSalida = Trifurcacion {
    izquierda = Nothing,
    derecha = Nothing,
    recto = Nothing
}

-- | Un laberinto vacio es una 'Trifurcacion' en la que todos sus caminos llevan a @Nothing@
laberintoVacio :: Laberinto
laberintoVacio = Left caminoSinSalida

{- | Función que crea un Tesoro dada la descripcion y el laberinto
-- Ejemplos:

@
>>> crearTesoro "El mejor tesoro del mundo" Nothing
Tesoro {descripcion = "El mejor tesoro del mundo", laberinto = Nothing}
>>> crearTesoro "El mejor tesoro del mundo v2.0" Just $ Trifurcacion {...}
Tesoro {descripcion = "El mejor tesoro del mundo v2.0", laberinto = Just (Trifurcacion {...})})
@
-}
crearTesoro :: String -- ^ Nombre del tesoro
    -> Maybe Laberinto -- ^ Posible laberinto que se seguira cuando se ignore el tesoro
    -> Tesoro -- 'Tesoro' tal que su descripcion y laberinto son los que se pasaron anteriormente
crearTesoro descripcion laberinto = Tesoro {
    descripcion = descripcion,
    laberinto = laberinto
}

{- | Función que una una trifurcacion y un laberinto mediante una direccion especificada.
-}
alterarTrifurcacion :: Trifurcacion -- ^ Trifurcacion que se va a "alterar"
    -> Maybe Laberinto -- ^ Nuevo @Maybe 'Laberinto'@ que se va a pegar en la direccion especificada desde la trifuracion
    -> Direccion -- ^ Direccion que se va a alterar de la trifurcacion
    -> Trifurcacion -- ^ Nueva trifurcacion alterada
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

{- | Función que permite acceder a un atributo de una trifurcacion
-- dado el lado que se desea explorar
-}
obtenerLabDeTrifur :: Trifurcacion -- ^ Trifurcacion de interes
    -> Direccion -- ^ Direccion que nos interesa acceder
    -> Maybe Laberinto -- ^ Lo que se encuentra hacia la direccion especificada
obtenerLabDeTrifur t Izquierda = izquierda t
obtenerLabDeTrifur t Derecha = derecha t
obtenerLabDeTrifur t Recto = recto t

{- | Función que dado un tesoro y una direccion, retorna lo que se encontraria
 si sigue esa direccion.
-}
obtenerLabDeTesoro :: Tesoro -- ^ Tesoro de interes
    -> Direccion -- ^ Direccion que nos interesa acceder
    -> Maybe Laberinto -- ^ Lo que se encuentra hacia la dirección especificada
obtenerLabDeTesoro t Recto = laberinto t
obtenerLabDeTesoro _ _ = Nothing

{- | Función que dado un laberinto y una direccion, retorna un Maybe laberinto
 indicando que se descubrira
-}
obtenerLaberintoPorDir :: Laberinto -- ^ Laberinto de interés
    -> Direccion -- ^ Dirección que nos interesa acceder
    -> Maybe Laberinto -- ^ Lo que se encuentra hacia la dirección especificada
obtenerLaberintoPorDir lab dir = case lab of
    -- Cuando es una trifurcacion, puedo retornar lo que diga la direccion
    Left trifur -> obtenerLabDeTrifur trifur dir
    Right tesoro -> obtenerLabDeTesoro tesoro dir

{- | Función que dado un laberinto me da el laberinto (o no) que conseguira
 si se dirige a la izquierda
-}
izquierdaLab :: Laberinto -- ^ Laberinto de interés
    -> Maybe Laberinto -- ^ Lo que se encuentra hacia la izquierda
izquierdaLab = flip obtenerLaberintoPorDir $ Izquierda

{- | Función que dado un laberinto me da el laberinto (o no) que conseguira
 si se dirige a la derecha
-}
derechaLab :: Laberinto -- ^ Laberinto de interés
    -> Maybe Laberinto -- ^ Lo que se encuentra hacia la derecha
derechaLab = flip obtenerLaberintoPorDir $ Derecha

{- | Función que dado un laberinto me da el laberinto (o no) que conseguira
 si sigue derecho
-}
rectoLab :: Laberinto -- ^ Laberinto de interés
    -> Maybe Laberinto -- ^ Lo que se encuentra si seguimos derecho
rectoLab = flip obtenerLaberintoPorDir $ Recto

---------------------------
-- Funciones de utilidad --
---------------------------

{- | Función que recibe una cadena de direcciones y construye un laberinto a partir
de esa ruta. El laberinto resultante estará compuesto de trifurcaciones, no tesoros.
Ejemplo:

@
>>> construirLaberintoDeRuta [Recto, Recto]
Left (Trifurcacion {
    izquierda = Nothing, derecha = Nothing, recto = Just (Left (Trifurcacion {
        izquierda = Nothing, derecha = Nothing, recto = Just (Left (Trifurcacion {
            izquierda = Nothing, derecha = Nothing
        }))
    }))
})
@
-}
construirLaberintoDeRuta :: Ruta -- ^ Ruta a expandir
    -> Laberinto -- ^ Laberinto resultante de seguir la ruta.
construirLaberintoDeRuta [] = Left caminoSinSalida
construirLaberintoDeRuta (x:xs) =
    Left $ alterarTrifurcacion caminoSinSalida (Just $ construirLaberintoDeRuta xs) x


{- | Función que recorre el laberinto hasta que encuentre una pared.
Cuando se consiga, se modifica el laberinto de modo que se extienda
el laberinto con la ruta restante.
-}
abrirRutaPared :: Laberinto -- ^ Laberinto que se esta recorriendo
    -> Ruta -- ^ Ruta que se esta siguiendo
    -> Laberinto -- ^ Laberinto basado en el anterior pero con posibles
                -- trifurcaciones nuevas resultantes de abrir paredes
abrirRutaPared l [] = l
abrirRutaPared l (x:xs) =
    -- Extraemos la trifurcacion
    let labSiguiente = obtenerLaberintoPorDir l x
        laberintoAPegar = case labSiguiente of
            -- Llegue a una pared, me toca construir un nuevo laberinto
            Nothing -> construirLaberintoDeRuta xs
            -- Puedo seguir recorriendo
            Just lab -> abrirRutaPared lab xs
    in case l of
        Left trif -> Left $ alterarTrifurcacion trif (Just laberintoAPegar) x
        Right tesoro -> Right $ crearTesoro (descripcion tesoro) (Just laberintoAPegar)

{- | Función que dada una direccion, un laberinto y una ruta pone en Nothing
la direccion en el laberinto luego de recorrer la ruta
-}
derrumbarPared :: Laberinto -- ^ Laberinto que se está recorriendo
    -> Ruta -- ^ Ruta que se esta siguiendo
    -> Direccion  -- ^ Direccion en la que se va a poner @Nothing@ cuando se termine de recorrer
    -> Maybe Laberinto -- ^ Laberinto envuelto en Maybe. La unica razón de hacer esto es para facilitar
                        -- el pasaje de argumentos a las funciones que usa esta.
derrumbarPared l [] d =
    let Left trif = l in Just $ Left $ alterarTrifurcacion trif Nothing d
derrumbarPared l (x:xs) d =
    let labASeguir = obtenerLaberintoPorDir l x
        Left trif = l
        in Just $ case labASeguir of
            Just lab -> Left $ alterarTrifurcacion trif (derrumbarPared lab xs d) x
            Nothing -> Left $ alterarTrifurcacion trif (derrumbarPared l xs d) x

{- | Función que dado un laberinto y una ruta determina si al final de la ruta
el usuario se encontrara en un tesoro o no.
-}
caigoEnTesoro :: Laberinto -- ^ Laberinto que se desea recorrer
    -> Ruta -- ^ Ruta que se desea recorrer
    -> Bool -- ^ Hay un tesoro al final del recorrido
caigoEnTesoro (Right tesoro) [] = True
caigoEnTesoro (Left trif) [] = False
caigoEnTesoro l (r:rs) =
    let labASeguir = obtenerLaberintoPorDir l r in
        case labASeguir of
            Just lab -> True && caigoEnTesoro lab rs
            Nothing -> True && caigoEnTesoro l rs

{- | Función que toma un laberinto, una ruta y si al final de esta hay un tesoro,
se elimina y si habia un camino despues del mismo se reemplaza con este.
La precondicion es que la ruta que se siga en el laberinto __lleve a un tesoro
cuando la ruta este vacia__
-}
tomarTesoro :: Laberinto -- ^ Laberinto que se esta recorriendo
    -> Ruta -- ^ Ruta a seguir
    -> Laberinto -- Laberinto tal que el tesoro al final ya no esta y se realizaron las conexiones pertinentes.
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

{- | Función que recorre una ruta y anade un tesoro justo en entre
el laberinto anterior a terminar la ruta y el siguiente (si hay).
Como precondicion establezco que debe haber al menos una ruta
-}
ponerTesoro :: Laberinto -- ^ Laberinto que se esta recorriendo
    -> Ruta -- ^ Ruta que se esta siguiendo
    -> Laberinto -- ^ Laberinto con el tesoro al final de la ruta
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
        -- Es una pared
        Nothing -> ponerTesoro l xs
        -- Hay que ver si el siguiente laberinto es un tesoro o un laberinto
        Just lab -> let nuevoLaberinto = Just $ ponerTesoro lab xs in case l of
            Left trif -> Left $ alterarTrifurcacion trif nuevoLaberinto x
            Right tesoro -> Right $ crearTesoro (descripcion tesoro) nuevoLaberinto

{- | Función que dado un laberinto y una ruta imprime el tesoro que hay al final
de esa ruta. Como precondicion __debe existir un tesoro al final de la ruta__
-}
imprimirTesoro :: Laberinto -- ^ Laberinto que se está recorriendo
    -> Ruta -- ^ Ruta que se esta siguiendo
    -> String -- ^ Descripcion del tesoro encontrado al final del recorrido
-- Caso base, cuando encontremos el tesoro devolvemos la descripcion de este
imprimirTesoro (Right tesoro) [] = descripcion tesoro
-- Caso recursivo, recorremos hacia la direccion que indique
imprimirTesoro l (x:xs) = let labSiguiente = obtenerLaberintoPorDir l x in
    case labSiguiente of
        Nothing -> imprimirTesoro l xs
        Just l' -> imprimirTesoro l' xs

-- | Función que verifica si al final de la ruta se termina en un camino sin salida
esRutaSinSalida :: Laberinto -- ^ Laberinto que se está recorriendo
    -> Ruta -- ^ Ruta que se esta siguiendo
    -> Bool -- ^ Es una ruta sin salida
esRutaSinSalida l [] =
    let l0 = izquierdaLab l
        l1 = derechaLab l
        l2 = rectoLab l
    in case (l0, l1, l2) of
        (Nothing, Nothing, Nothing) -> True
        _ -> False

esRutaSinSalida l (x:xs) =
    let labSiguiente = obtenerLaberintoPorDir l x
    in case labSiguiente of
        Nothing -> esRutaSinSalida l xs
        Just lab -> esRutaSinSalida lab xs

