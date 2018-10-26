module Main (main) where
import Laberinto

imprimirMenu :: IO ()
imprimirMenu = do
    putStrLn "1) Hablar de un laberinto nuevo"
    putStrLn "2) Quiero darte una ruta"
    putStrLn "3) Reportar pared abierta"
    putStrLn "4) Reportar derrumbe"
    putStrLn "5) Reportar tesoro tomado"
    putStrLn "6) Reportar tesoro hallado"
    putStrLn "7) Dar nombre al laberinto"
    putStrLn "8) Hablar de un laberinto de nombre conocido"

main :: IO ()
main = do
    putStrLn "¡Hola!"
    putStrLn "Soy el sabio del laberinto, ¿me indicas qué deseas hacer?"
    imprimirMenu
