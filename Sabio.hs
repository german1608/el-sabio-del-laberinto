module Main (main) where
import Laberinto
import System.IO (writeFile)
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import qualified Control.Monad.State as ST

type Sabio = ST.StateT (Laberinto, Ruta) IO ()

-- Alias para liftIO
io :: IO a -> ST.StateT x IO a
io = ST.liftIO

-- | Lista con tuplas para indicar opciones posibles y su mensaje para
-- indicar al usuario que se ejecutara cuando se escoga esa opcion.
-- Tambien se indica la funcion que se va a ejecutar en cada caso
opcionesPosiblesConMsj :: [(String, String, Sabio)]
opcionesPosiblesConMsj = [
    ("1", "Hablar de un laberinto nuevo", laberintoNuevo),
    ("2", "Quiero darte una ruta", return ()),
    ("3", "Reportar pared abierta", reportarParedAbierta),
    ("4", "Reportar derrumbe", reportarDerrumbe),
    ("5", "Reportar tesoro tomado", reportarTesoroTomado),
    ("6", "Reportar tesoro hallado", reportarTesoroHallado),
    ("7", "Dar nombre al laberinto", darNombreAlLaberinto),
    ("8", "Hablar de un laberinto de nombre conocido", return ())
    ]

-- | Opciones posibles para saber si el input del usuario es correcto
opciones :: [String]
opciones = map (\(x,_,_) -> x) opcionesPosiblesConMsj

-- | Funcion que imprime el menu con las posibles opciones
imprimirMenu :: Sabio
imprimirMenu = io $ putStr $
    foldl (\acc (x,y,_) -> acc ++ x ++ ") " ++ y ++ "\n") "" opcionesPosiblesConMsj

-- | Funcion que imprime las instrucciones para pedir la ruta al usuario
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


-- | Funcion que reemplaza el laberinto actual por el nuevo laberinto
-- que diga el usuario
laberintoNuevo :: Sabio
laberintoNuevo = do
    io imprimirInstrDeRuta
    ST.put (laberintoVacio, [])
    c <- io getLine
    ST.put (construirLaberintoDeRuta $ parsearRuta c, [])

-- | controlador de la opcion para reportar pared abierta
reportarParedAbierta :: Sabio
reportarParedAbierta = do
    -- Obtenemos el estado actual dl laberinto para recuperar el inicio
    -- cuando terminemos de ejecutar esto
    (labInicial, _) <- ST.get
    io imprimirInstrDeRuta -- Imprimimos instrucciones
    c <- io getLine -- pedimos input
    let ruta = parsearRuta c -- parseamos el string con la ruta
    ST.put (abrirRutaPared labInicial ruta, ruta) -- Reemplazamos el estado para que vaya consumiendo la ruta

-- | Controlador para la opcion para reportar derrumbe
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

-- | Controlador para la opcion de reportar tesoros tomados
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

-- | Controlador para la opcion de reportar tesoros hallado
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

-- | Controlador para dar nombre al laberinto
darNombreAlLaberinto :: Sabio
darNombreAlLaberinto = do
    io $ putStr "Escriba el nombre del laberinto: "
    nombre <- io getLine
    (lab, _) <- ST.get
    io $ writeFile nombre $ show lab
    io $ putStrLn "¡Archivo guardado!"


-- | Funcion que hace prompt al user por las opciones adecuadas
prompt :: Sabio
prompt = do
    (lab, _) <- ST.get
    io $ print lab
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

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    ST.runStateT prompt (laberintoVacio, [])
    return ()
