{-|
Module      : Sabio
Description : Main y uso de las funciones y tipos de datos usados en Laberinto
Copyright   : (c) German Robayo, 2018
                  Gabriel Gutierrez, 2018
Maintainer  : 14-10924@usb.ve, 13-10625@usb.ve

Contiene los tipos de datos y las funciones que se van a usar
para manipular estos.
-}
module Main where
import Laberinto hiding (obtenerLabDeTrifur)
import System.IO (writeFile, hSetBuffering, stdout, BufferMode(NoBuffering))
import System.Directory (doesFileExist)
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Control.Monad.State as ST

-- | Tipo de dato para recordar el estado del laberinto en cada recursión
-- y realizar acciones IO.
type Sabio = ST.StateT (Laberinto, Ruta) IO ()

-- | Alias para liftIO
io :: IO a -> ST.StateT x IO a
io = ST.liftIO

-- | Lista con tuplas para indicar opciones posibles y su mensaje para
-- indicar al usuario que se ejecutara cuando se escoga esa opcion.
-- Tambien se indica la función que se va a ejecutar en cada caso
opcionesPosiblesConMsj :: [(String, String, Sabio)]
opcionesPosiblesConMsj = [
    ("1", "Hablar de un laberinto nuevo", laberintoNuevo),
    ("2", "Quiero darte una ruta", preguntarRuta),
    ("3", "Reportar pared abierta", reportarParedAbierta),
    ("4", "Reportar derrumbe", reportarDerrumbe),
    ("5", "Reportar tesoro tomado", reportarTesoroTomado),
    ("6", "Reportar tesoro hallado", reportarTesoroHallado),
    ("7", "Dar nombre al laberinto", darNombreAlLaberinto),
    ("8", "Hablar de un laberinto de nombre conocido", cargarLaberintoDeArchivo)
    ]

-- | Opciones posibles para saber si el input del usuario es correcto
opciones :: [String]
opciones = map (\(x,_,_) -> x) opcionesPosiblesConMsj

-- | Función que imprime el menu con las posibles opciones
imprimirMenu :: Sabio
imprimirMenu = io $ putStr $
    (foldl (\acc (x,y,_) -> acc ++ x ++ ") " ++ y ++ "\n") "" opcionesPosiblesConMsj) ++
    "> "

-- | Función que imprime las instrucciones para pedir la ruta al usuario
imprimirInstrDeRuta :: IO ()
imprimirInstrDeRuta = do
    putStrLn "Introduzca la ruta:"
    putStrLn "Instrucciones:"
    putStrLn "\t(>) derecha"
    putStrLn "\t(<) izquierda"
    putStrLn "\t(^) recto"
    putStrLn "Ejemplo:"
    putStrLn "><^<^<"
    putStrLn "Si introduce un caracter erroneo en algun momento se ignorará"
    putStr "> "


-- | Función que reemplaza el laberinto actual por el nuevo laberinto
-- que diga el usuario. Leemos una ruta y a partir de ella construimos nuestro
-- laberinto nuevo usando 'Laberinto.construirLaberintoDeRuta'
laberintoNuevo :: Sabio
laberintoNuevo = do
    io imprimirInstrDeRuta
    ST.put (laberintoVacio, [])
    c <- io getLine
    ST.put (construirLaberintoDeRuta $ parsearRuta c, [])

-- | Controlador de la opcion para reportar pared abierta. Leemos una ruta por parte
-- del usuario y ejecutamos la funcion 'Laberinto.abrirRutaPared'
reportarParedAbierta :: Sabio
reportarParedAbierta = do
    -- Obtenemos el estado actual dl laberinto para recuperar el inicio
    -- cuando terminemos de ejecutar esto
    (labInicial, _) <- ST.get
    io imprimirInstrDeRuta -- Imprimimos instrucciones
    c <- io getLine -- pedimos input
    let ruta = parsearRuta c -- parseamos el string con la ruta
    ST.put (abrirRutaPared labInicial ruta, ruta) -- Reemplazamos el estado para que vaya consumiendo la ruta

-- | Controlador para la opcion para reportar derrumbe. Leemos una ruta por parte del
-- usuario, una direccion y ejecutamos la funcion 'Laberinto.derrumbarPared'
reportarDerrumbe :: Sabio
reportarDerrumbe = do
    -- Obtenemos el laberinto actual
    (labInicial, _) <- ST.get
    io imprimirInstrDeRuta
    c <- io getLine
    let ruta = parsearRuta c
    io $ putStrLn "Ahora dame una direccion (mismo formato que el anterior):"
    c <- io getLine
    let prepared = fromJust . derrumbarPared labInicial ruta
    case c of
        ">" -> ST.put(prepared Derecha, [])
        "<" -> ST.put(prepared Izquierda, [])
        "^" -> ST.put(prepared Recto, [])
        _ -> do
            io $ putStrLn "Opcion equivocada"
            reportarDerrumbe

-- | Controlador para la opcion de reportar tesoros tomados. Leemos una ruta por
-- parte del usuario y ejecutamos la funcion 'Laberinto.tomarTesoro'
reportarTesoroTomado :: Sabio
reportarTesoroTomado = do
    -- Obtenemos el laberinto actual
    (labInicial, _) <- ST.get
    io imprimirInstrDeRuta
    c <- io getLine
    let ruta = parsearRuta c
    if length ruta == 0 then do
        io $ putStrLn "La ruta no puede ser vacia"
        reportarTesoroTomado
    else if caigoEnTesoro labInicial ruta then
        ST.put (tomarTesoro labInicial ruta, [])
    else
        io $ putStrLn "La ruta suministrada no dirige a ningun tesoro"

-- | Controlador para la opcion de reportar tesoros hallado. Leemos una ruta
-- por parte del usuario y quita el tesoro al final de esta usando 'Laberinto.ponerTesoro'
reportarTesoroHallado :: Sabio
reportarTesoroHallado = do
    -- Obtenemos el laberinto actual
    (labInicial, _) <- ST.get
    io imprimirInstrDeRuta
    c <- io getLine
    let ruta = parsearRuta c
    if length ruta == 0 then do
        io $ putStrLn "La ruta no puede ser vacia"
        reportarTesoroHallado
    else if caigoEnTesoro labInicial ruta then
        io $ putStrLn "Ya hay un tesoro al final de la ruta"
    else
        ST.put (ponerTesoro labInicial ruta, [])

-- | Controlador para dar nombre al laberinto. Leemos el nombre un archivo y guardamos
-- la representacion en el mismo.
darNombreAlLaberinto :: Sabio
darNombreAlLaberinto = do
    io $ putStr "Escriba el nombre del laberinto: "
    nombre <- io getLine
    (lab, _) <- ST.get
    io $ writeFile nombre $ show lab
    io $ putStrLn "¡Archivo guardado!"

-- | Controlador para cargar el laberinto desde un archivo. Leemos el nombre de un archivo
-- y parseamos la representacion a uno que la computadora pueda entender.
cargarLaberintoDeArchivo :: Sabio
cargarLaberintoDeArchivo = do
    io $ putStr "Escriba el nombre del laberinto que desea cargar: "
    nombre <- io getLine
    archivoExiste <- io $ doesFileExist nombre
    if not archivoExiste then do
        io $ putStrLn "El archivo suministrado no existe"
        cargarLaberintoDeArchivo
    else do
        contents <- io $ readFile nombre
        ST.put (read contents, [])

-- | Función que recorre el laberinto con la ruta modificando el laberinto
-- a medida que seguimos.
recorrerLaberinto :: Sabio
recorrerLaberinto = do
    (lab, ruta) <- ST.get
    case ruta of
        [] -> return ()
        (r:rs) -> do
            let l = obtenerLaberintoPorDir lab r
            case l of
                Nothing -> ST.put (lab, rs)
                Just lab' -> ST.put (lab', rs)
            recorrerLaberinto

-- | Funcion que que pide input sobre las rutas del usuario y le
-- indicasi hay tesoros, rutas sin salidas o si es posible cambiar la misma.
ejecutarRuta :: Sabio
ejecutarRuta = do
    (labInicial, ruta) <- ST.get
    if caigoEnTesoro labInicial ruta then do
        io $ putStr "¡Se ha encontrado el tesoro \""
        io $ putStr $ imprimirTesoro labInicial ruta
        io $ putStrLn "\"!"
    else if esRutaSinSalida labInicial ruta then
        io $ putStrLn "¡Has llegado a una ruta sin salida!"
    else do
        ST.put (labInicial, ruta)
        recorrerLaberinto

        io $ putStrLn "Tu ruta no ha llevado a un tesoro o a un camino sin salida"
        io $ putStrLn "Escoja una de las siguientes alternativas"
        io $ putStrLn "1) Continuar ruta"
        io $ putStrLn "2) Preguntar nueva ruta"
        opcion <- io getLine
        case opcion of
            "1" -> do
                (lab, _) <- ST.get
                ST.put(lab, [])
                preguntarRuta
            "2" -> do
                ST.put(labInicial, [])
                preguntarRuta
            _ -> do
                io $ putStrLn "Opcion equivocada"

-- | Controlador para preguntar la ruta al usuario. Establece una forma de recuperar
-- el laberinto inicial cuando se termina la interaccion con el usuario
preguntarRuta :: Sabio
preguntarRuta = do
    io imprimirInstrDeRuta
    c <- io getLine
    let ruta = parsearRuta c
    (labInicial, _) <- ST.get
    ST.put(labInicial, ruta)
    ejecutarRuta
    ST.put(labInicial, [])


-- | Función que hace prompt al user por las opciones adecuadas.
prompt :: Sabio
prompt = do
    (lab, _) <- ST.get
    imprimirMenu
    opcion <- io getLine
    if not $ opcion `elem` opciones then do
        io $ putStrLn "Opción incorrecta"
        io $ putStr "Las opciones correctas son: "
        io $ putStrLn $ intercalate ", "  opciones
    else do
        -- Obtenemos la accion a ejecutar
        let [(_, _, accion)] = filter (\(x,_,_) -> x == opcion) opcionesPosiblesConMsj
        accion
    prompt

-- | Funcion que se va a ejecutar cuando se invoque el ejecutable final
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    ST.runStateT prompt (laberintoVacio, [])
    return ()
