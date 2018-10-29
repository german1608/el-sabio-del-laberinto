module Main (main) where
import Laberinto
import System.IO (hSetBuffering, stdout, BufferMode(NoBuffering))
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)
import qualified Control.Monad.State as ST

type Ruta = [Direccion]
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
    ("3", "Reportar pared abierta", return ()),
    ("4", "Reportar derrumbe", return ()),
    ("5", "Reportar tesoro tomado", return ()),
    ("6", "Reportar tesoro hallado", return ()),
    ("7", "Dar nombre al laberinto", return ()),
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

-- | Funcion que dada una ruta del usuario construye un laberinto
-- que contiene solo esa ruta. La ruta debe estar al reves
construirLaberintoDeRuta :: [Direccion] -> Sabio
construirLaberintoDeRuta [] = return ()
construirLaberintoDeRuta (x:xs) = do
    (lab, ruta) <- ST.get
    let trif = alterarTrifurcacion caminoSinSalida lab x
    ST.put (Left trif, xs)
    construirLaberintoDeRuta xs

-- | Funcion que reemplaza el laberinto actual por el nuevo laberinto
-- que diga el usuario
laberintoNuevo :: Sabio
laberintoNuevo = do
    io $ imprimirInstrDeRuta
    ST.put $ (laberintoVacio, [])
    c <- io getLine
    construirLaberintoDeRuta $ reverse $ parsearRuta c

-- | Funcion que hace prompt al user por las opciones adecuadas
prompt :: Sabio
prompt = do
    imprimirMenu
    opcion <- io getLine
    if not $ opcion `elem` opciones then do
        io $ putStrLn "Opción incorrecta"
        io $ putStr "Las opciones correctas son: "
        io $ putStrLn $ intercalate ", "  opciones
    else do
        let [(_, _, accion)] = filter (\(x,_,_) -> x == opcion) opcionesPosiblesConMsj
        accion
    prompt

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    ST.runStateT prompt (laberintoVacio, [])
    return ()
