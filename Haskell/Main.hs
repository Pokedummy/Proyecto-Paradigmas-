{-
  ST0244 - Paradigmas de Programacion
  Practica I - Del pixel a la integral: area bajo la curva
  Parte funcional (Haskell)

  Pipeline conceptual:
    PBM -> bytes -> pixeles -> f(x) -> M -> area

  Compilar:   ghc -O2 Main.hs -o area
  Ejecutar:   ./area ../curva_binaria_P4.pbm
  (o sin compilar: runghc Main.hs ../curva_binaria_P4.pbm)

  Requiere el paquete 'bytestring' (viene incluido con GHC).
-}

module Main where

import qualified Data.ByteString as BS
import Data.Bits (testBit)
import Data.Word (Word8)
import System.Environment (getArgs)
import System.IO (hSetEncoding, stdout, utf8)
import Control.Monad (forM_)
import Text.Printf (printf)

-- ============================================================
-- Estructura que representa la imagen ya parseada
-- ============================================================

data PBM = PBM
  { pbmWidth    :: Int
  , pbmHeight   :: Int
  , pbmData     :: BS.ByteString  -- cuerpo binario (bits empacados en bytes)
  , pbmRowBytes :: Int            -- bytes por fila = ceil(ancho / 8)
  }

-- ============================================================
-- 1. CARGAR EL ARCHIVO PBM P4
-- ============================================================

loadPBM :: FilePath -> IO PBM
loadPBM path = do
  bs <- BS.readFile path
  let (magic, rest1) = readToken bs
  if magic /= "P4"
    then error "El archivo no tiene el formato PBM P4 esperado"
    else do
      let (wStr, rest2)  = readToken rest1
          (hStr, rest3)  = readToken rest2
          width          = read wStr  :: Int
          height         = read hStr  :: Int
          pixelData      = BS.drop 1 rest3  -- se descarta el unico espacio antes del binario
          rowBytes       = (width + 7) `div` 8
      return (PBM width height pixelData rowBytes)

-- Lee un token separado por espacios, saltando espacios en blanco y comentarios (#...)
readToken :: BS.ByteString -> (String, BS.ByteString)
readToken bs =
  let bs'            = skipWhitespaceAndComments bs
      (tokBytes, rst) = BS.span (not . isSpaceByte) bs'
  in (map (toEnum . fromIntegral) (BS.unpack tokBytes), rst)

isSpaceByte :: Word8 -> Bool
isSpaceByte w = w `elem` [32, 9, 10, 13]  -- espacio, tab, \n, \r

skipWhitespaceAndComments :: BS.ByteString -> BS.ByteString
skipWhitespaceAndComments bs
  | BS.null bs                 = bs
  | isSpaceByte (BS.head bs)   = skipWhitespaceAndComments (BS.drop 1 bs)
  | BS.head bs == 35           = skipWhitespaceAndComments (BS.dropWhile (/= 10) bs) -- '#'
  | otherwise                  = bs

-- ============================================================
-- 2. ACCESO A PIXELES INDIVIDUALES
-- ============================================================

-- isBlack img x y = True si el pixel (x,y) es negro (bit = 1)
-- Empaquetado: MSB primero, filas alineadas a byte (estandar PBM P4)
isBlack :: PBM -> Int -> Int -> Bool
isBlack img x y =
  let byteIndex = y * pbmRowBytes img + (x `div` 8)
      bitIndex  = 7 - (x `mod` 8)
      byte      = BS.index (pbmData img) byteIndex
  in testBit byte bitIndex

-- ============================================================
-- 3. LA FUNCION DISCRETA f(x)
--    Cuenta pixeles negros consecutivos desde abajo hacia arriba
-- ============================================================

f :: PBM -> Int -> Int
f img x =
  length (takeWhile id [ isBlack img x y | y <- [pbmHeight img - 1, pbmHeight img - 2 .. 0] ])

-- ============================================================
-- 4. ESTRUCTURA DE ALTURAS
--    M = [f(0), f(1), ..., f(n-1)]  <-  aplicar f a todo el dominio
-- ============================================================

heights :: PBM -> [Int]
heights img = map (f img) [0 .. pbmWidth img - 1]

-- ============================================================
-- 5. AREA MEDIANTE SUMA DE RIEMANN (Ax = 1 pixel)
--    area = sum M
-- ============================================================

area :: [Int] -> Int
area = sum

-- ============================================================
-- 6. VISUALIZACION DE LA CURVA EN CONSOLA (imagen escalada)
--    Se agrupan columnas en "cubetas" y se dibuja una barra por cubeta
-- ============================================================

visualizeCurve :: [Int] -> Int -> Int -> IO ()
visualizeCurve m targetWidth targetHeight = do
  let n         = length m
      groupSize = max 1 (n `div` targetWidth)
      buckets   = chunk groupSize m
      sampled   = map avg buckets
      maxH      = maximum sampled
      scaled    = map (scaleTo maxH targetHeight) sampled
  forM_ [targetHeight, targetHeight - 1 .. 1] $ \row ->
    putStrLn [ if s >= row then '#' else ' ' | s <- scaled ]
  putStrLn (replicate (length scaled) '-')

scaleTo :: Int -> Int -> Int -> Int
scaleTo maxH targetHeight h
  | maxH == 0 = 0
  | otherwise = round (fromIntegral h / fromIntegral maxH * fromIntegral targetHeight :: Double)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

avg :: [Int] -> Int
avg [] = 0
avg xs = sum xs `div` length xs

-- ============================================================
-- 7. VISUALIZACION DE M[x] = f(x) CON BLOQUES UNICODE (sparkline)
-- ============================================================

sparkline :: [Int] -> Int -> String
sparkline m targetWidth =
  let n         = length m
      groupSize = max 1 (n `div` targetWidth)
      buckets   = chunk groupSize m
      sampled   = map avg buckets
      maxH      = maximum sampled
      blocks    = " ▁▂▃▄▅▆▇█"
      levels    = length blocks - 1
      toBlock h = if maxH == 0
                    then ' '
                    else blocks !! round (fromIntegral h / fromIntegral maxH * fromIntegral levels :: Double)
  in map toBlock sampled

-- ============================================================
-- 8. VALORES DE MUESTRA x_i -> f(x_i)
-- ============================================================

sampleValues :: [Int] -> Int -> [(Int, Int)]
sampleValues m count =
  let n    = length m
      step = max 1 (n `div` count)
      idxs = take count [0, step .. n - 1]
  in [ (i, m !! i) | i <- idxs ]

-- ============================================================
-- PROGRAMA PRINCIPAL
-- ============================================================

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  let path = case args of
               (p:_) -> p
               []    -> "curva_binaria_P4.pbm"

  img <- loadPBM path
  printf "Imagen: %d x %d pixeles\n" (pbmWidth img) (pbmHeight img)

  let m = heights img
      a = area m

  putStrLn "\n=== VISUALIZACION DE LA CURVA (imagen escalada) ==="
  visualizeCurve m 100 25

  putStrLn "\n=== FUNCION DE ALTURAS M[x] = f(x) (bloques unicode) ==="
  putStrLn (sparkline m 100)

  putStrLn "\n=== VALORES DE MUESTRA x_i -> f(x_i) ==="
  forM_ (sampleValues m 10) $ \(x, h) ->
    printf "x_%d = %d  ->  f(x_%d) = %d pixeles\n" x x x h

  putStrLn "\nCada columna tiene base = 1 pixel"
  putStrLn "Area = suma de f(x_i)"
  printf "Area = %d pixeles cuadrados\n" a
