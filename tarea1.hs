-- Tarea 1: Práctica de Haskell [cite: 3]
-- CI-3661 Laboratorio de Lenguajes de Programación I [cite: 2]
-- Miguel Salomon
--Carnet: 1910274

module Tarea1 where

import Text.Read (readMaybe)
import Data.Char (toUpper)
import Data.List (foldl')

-- | Problema 1: Palíndromo [cite: 4]
-- | Implementa una función que determine si una cadena es un palíndromo
-- | usando recursión explícita[cite: 5, 6].
esPalindromo :: String -> Bool
esPalindromo [] = True  -- Un string vacío es palíndromo
esPalindromo [_] = True -- Un string de un solo caracter es palíndromo
esPalindromo (x:xs) = (x == last xs) && esPalindromo (init xs) -- Compara el primero y el último, y chequea recursivamente el interior


-- | Problema 2: Producto de Elementos Pares en una Lista [cite: 10]
-- | Calcula el producto de los elementos pares en una lista de enteros
-- | usando recursión explícita[cite: 11].
-- | Devuelve 1 si la lista está vacía o no contiene pares[cite: 12].
productoParesRec:: [Integer] -> Integer
productoParesRec [] = 1 -- Caso base: lista vacía [cite: 12]
productoParesRec (x:xs)
    | even x    = x * productoParesRec xs -- Si es par, lo multiplica
    | otherwise = productoParesRec xs     -- Si es impar, lo ignora


-- | Problema 3: Parseo Condicional con Either [cite: 15]
-- | Intenta convertir cada cadena de una lista a Int[cite: 16].
-- | Devuelve Right Int si es exitoso[cite: 18].
-- | Devuelve Left String (en mayúsculas) si falla[cite: 17, 18].
parsearCondicional:: [String] -> [Either String Int]
parsearCondicional = map parseSingle
  where
    parseSingle :: String -> Either String Int
    parseSingle str =
      case readMaybe str :: Maybe Int of
        Just num -> Right num -- Conversión exitosa [cite: 17]
        Nothing  -> Left (map toUpper str) -- Conversión fallida [cite: 17]


-- | Problema 4: Suma Acumulada Condicional [cite: 22]
-- | Filtra una lista de Floats, manteniendo solo los > umbral[cite: 23, 24].
-- | Luego, calcula la suma de los filtrados usando un plegado (fold)[cite: 24].
sumaAcumuladaCondicional :: Float -> [Float] -> Float
sumaAcumuladaCondicional umbral xs =
  let filtrados = filter (> umbral) xs -- Filtra los números
  in foldl' (+) 0 filtrados           -- Suma los resultados con un fold


-- | Problema 5: Generación de Coordenadas Impares [cite: 27]
-- | Genera una lista de pares (x, y) donde x e y están en [1, N]
-- | y la suma (x + y) es impar[cite: 28].
-- | Utiliza Listas por Comprensión[cite: 29].
coordenadasImpares :: Int -> [(Int, Int)]
coordenadasImpares n = [(x, y) | x <- [1..n], y <- [1..n], odd (x + y)]


-- | Problema 6: Descomposición Segura de Lista [cite: 32]
-- | Divide una lista en su cabeza (head) y cola (tail) de forma segura
-- | usando el tipo Maybe[cite: 33].
-- | Devuelve Nothing si la lista está vacía[cite: 34].
-- | Devuelve Just (cabeza, cola) si no está vacía[cite: 34].
descomponerListaSegura :: [a] -> Maybe (a, [a])
descomponerListaSegura []     = Nothing -- Caso lista vacía [cite: 34]
descomponerListaSegura (x:xs) = Just (x, xs) -- Caso lista no vacía [cite: 34]