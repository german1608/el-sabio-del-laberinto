module Main (main) where
import Laberinto
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import qualified Control.Monad.State as ST

type Ruta = [Direccion]
type Sabio = ST.StateT (Laberinto, Ruta) IO ()

-- Alias para liftIO
io :: IO a -> ST.StateT x IO a
io = ST.liftIO

-- | Lista con tuplas para indicar opciones posibles y su mensaje para
-- indicar al usuario que se ejecutara cuando se escoga esa opcion
opcionesPosiblesConMsj :: [(String, String)]
opcionesPosiblesConMsj = [
    ("1", "Hablar de un laberinto nuevo"),
    ("2", "Quiero darte una ruta"),
    ("3", "Reportar pared abierta"),
    ("4", "Reportar derrumbe"),
    ("5", "Reportar tesoro tomado"),
    ("6", "Reportar tesoro hallado"),
    ("7", "Dar nombre al laberinto"),
    ("8", "Hablar de un laberinto de nombre conocido")
    ]

-- | Opciones posibles para saber si el input del usuario es correcto
opciones :: [String]
opciones = map fst opcionesPosiblesConMsj

-- | Funcion que imprime el menu con las posibles opciones
imprimirMenu :: Sabio
imprimirMenu = io $ putStr $
    foldl (\acc (x,y) -> acc ++ x ++ ") " ++ y ++ "\n") "" opcionesPosiblesConMsj

imprimirInstrDeRuta :: IO ()
imprimirInstrDeRuta = do
    putStrLn "Introduzca la ruta:"
    putStrLn "Instrucciones:"
    putStrLn "\t(>) derecha"
    putStrLn "\t(<) izquierda"
    putStrLn "\t(^) recto"
    putStrLn "\t(x) fin"
    putStrLn "Ejemplo:"
    putStrLn "><^<^<x"
    putStrLn "Si introduce un caracter erroneo en algun momento no se pierde la ruta"

leerRuta :: Sabio
leerRuta = do
    c <- io getChar
    io $ print c
    if not $ c `elem` ['>', '<', '^', 'x'] then do
        io $ putStrLn "Caracter inválido."
        leerRuta
    else do
        (lab, ruta) <- ST.get
        case c of
            'x' -> return ()
            '>' -> do
                ST.put $ (lab, Derecha:ruta)
                leerRuta
            '<' -> do
                ST.put $ (lab, Izquierda:ruta)
                leerRuta
            '^' -> do
                ST.put $ (lab, Recto:ruta)
                leerRuta

-- | Funcion que reemplaza el laberinto actual por el nuevo laberinto
-- que diga el usuario
laberintoNuevo :: Sabio
laberintoNuevo = do
    io $ imprimirInstrDeRuta
    ST.put $ (laberintoVacio, [])
    leerRuta
    io $ putStrLn "listo"
    (_, ruta) <- ST.get
    io $ print ruta

-- | Funcion que hace prompt al user por las opciones adecuadas
prompt :: Sabio
prompt = do
    imprimirMenu
    opcion <- io getLine
    if not $ opcion `elem` opciones then do
        io $ putStrLn "Opción incorrecta"
        io $ putStr "Las opciones correctas son: "
        io $ putStrLn $ intercalate ", "  opciones
    else
        laberintoNuevo
    prompt

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    ST.runStateT prompt (laberintoVacio, [])
    return ()
