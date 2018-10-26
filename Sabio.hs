module Main (main) where
import Laberinto
import Data.Char (digitToInt, isDigit)
import Data.List (intercalate)

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
imprimirMenu :: IO ()
imprimirMenu = putStr $
    foldl (\acc (x,y) -> acc ++ x ++ ") " ++ y ++ "\n") "" opcionesPosiblesConMsj

-- | Funcion que hace prompt al user por las opciones adecuadas
prompt :: IO ()
prompt = do
    imprimirMenu
    opcion <- getLine
    if not $ opcion `elem` opciones then do
        putStrLn "Opción incorrecta"
        putStr "Las opciones correctas son: "
        putStrLn $ intercalate ", "  opciones
    else
        putStr "op"
    prompt

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    prompt
