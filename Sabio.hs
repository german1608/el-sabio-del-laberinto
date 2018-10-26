module Main (main) where
import Laberinto

opcionesPosiblesConMsj :: [(Char, String)]
opcionesPosiblesConMsj = [('1', "Hablar de un laberinto nuevo"),
    ('2', "Quiero darte una ruta"),
    ('3', "Reportar pared abierta"),
    ('4', "Reportar derrumbe"),
    ('5', "Reportar tesoro tomado"),
    ('6', "Reportar tesoro hallado"),
    ('7', "Dar nombre al laberinto"),
    ('8', "Hablar de un laberinto de nombre conocido")]

-- | Funcion que imprime el menu con las posibles opciones
imprimirMenu :: IO ()
imprimirMenu = putStr $
    foldl (\acc (x,y) -> acc ++ [x] ++ ") " ++ y ++ "\n") "" opcionesPosiblesConMsj

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    imprimirMenu
