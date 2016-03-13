

-- Importamos Data.Char para poder usar toLower
import Data.Char
-- Importamos Data.List para poder usar sortBy y elemIndex
import Data.List
import Data.Maybe
-- Importamos Text.Read para poder usar readMaybe. Antes se encontraba en Data.Maybe, pero en nuevas versiones está aquí
import Text.Read

-- Importamos Data.Map. Dado que los nombres de sus funciones coinciden con otras funciones de listas,
-- indicamos que las funciones y clases importadas tendrán sus nombres precedidos por "Map.".
import qualified Data.Map as Map


muestraMenu :: IO ()
muestraMenu = do
-- Imprimimos las opciones
  putStrLn "1. Mostrar texto cifrado"
  putStrLn "2. Mostrar descifrado actual"
  putStrLn "3. Cambiar una letra de la combinacion"
  putStrLn "4. Ver estadísticas de frecuencias"
  putStrLn "5. Mostrar posibilidades para una palabra"
  putStrLn "6. Ejecutar descifrado automatico"
  putStrLn "0. Salir"


-- Pide un caracter por consola
-- Si se inserta mas de uno, se elige el primero y el resto sera ignorado, pero procesado
-- Se espera hasta salto de linea
pideCaracter = do
  c <- getChar
  if c == '\n'
    then do
      c <- pideCaracter
      return c
    else do
      getLine
      return c

      
-- Función para iniciar el menú con los valores por defecto
-- Se ejecuta automáticamente al abrir el ejecutable
main = do
  let combinacionInicial = ['_' | i<-['a'..'z']]
  bucleMenu combinacionInicial Nothing
  putStrLn "Fin"

  
-- Muestra la combinación/biyección actual
muestraCombinacionActual combinacion = do
  putStrLn "Combinacion actual: "
  putStr " Cifrado: "
  print ['a'..'z']
  putStr " Descif.: "
  print combinacion
  putStrLn ""

  
-- fuerzaMapa: devuelve un mapa de las palabras del español numeradas
--  si el parámetro es Nothing, se crea el mapa y se devuelve
--  de lo contrario, se devuelve el parámetro (puesto que ya era el mapa)
-- Ejemplo de numeración: alcachofa -> 123134561
fuerzaMapa (Just mapa) = do
  return mapa
  
fuerzaMapa Nothing = do
  palabrasNumeradasEspanol <- numeraPalabrasEspanol
  let mPalabrasNumeradasEspanol = Map.fromListWith (++) (reverse palabrasNumeradasEspanol)
  putStrLn "Cargando lista de palabras en memoria (esta operación puede tardar un rato)..."
  putStrLn ( (show (Map.size mPalabrasNumeradasEspanol)) ++ " palabras cargadas")
  return mPalabrasNumeradasEspanol
  
  
-- Bucle continuo que muestra el menú hasta que se inserte un 0
bucleMenu combinacion mapa = do
  muestraCombinacionActual combinacion
  muestraMenu
  
  -- Pedimos una opción
  putStrLn "Elige una opcion: "
  opcion <- pideCaracter
  
  -- Si no es un 0, procesamos
  if opcion /= '0'
    then do
      putStrLn ""
      
      if elem opcion ['1'..'6'] then do
        case opcion of
          
          -- Opción 1: mostramos el texto codificado
          '1' -> do 
            textoCodificado <- leeTextoCodificado
            putStrLn "-- Texto codificado (original) --"
            putStrLn textoCodificado
            bucleMenu combinacion mapa
          
          -- Opción 2: mostramos el texto codificado, descifrado con la combinación actual
          '2' -> do 
            textoCodificado <- leeTextoCodificado
            putStrLn "-- Descifrado actual --"
            putStrLn (usaCombinacion textoCodificado combinacion)
            bucleMenu combinacion mapa
          
          -- Opción 3: cambiamos una letra de la combinación (es decir, cambiamos que x letra sea reemplazada por y al descifrar)
          '3' -> do
            putStrLn "Elige letra original (a-z): "
            l <- pideCaracter
            
            if elem l ['a'..'z'] then do
              putStrLn "Elige reemplazo: "
              r <- pideCaracter
              let posicion = fromMaybe 0 (elemIndex l ['a'..'z'])
              let combinacion2 = (take posicion combinacion) ++ [r] ++ (drop (posicion+1) combinacion)
              bucleMenu combinacion2 mapa
            else do
              putStrLn "Error: has insertado un caracter invalido"
              bucleMenu combinacion mapa
          
          -- Opción 4: mostramos las frecuencias de letras del español y del texto codificado
          '4' -> do 
            frecuenciasEspanol <- calculaFrecuenciasEspanol
            frecuenciasTexto <- calculaFrecuenciasTexto
            let letrasEspanol = listaLetrasPorFrecuencia frecuenciasEspanol
            let letrasCodificado = listaLetrasPorFrecuencia frecuenciasTexto
            
            putStr "Letras más usadas del texto codificado: "
            putStrLn letrasCodificado
            putStr "Letras más usadas del español:          "
            putStrLn letrasEspanol
            putStrLn ""
            
            let combinacionNueva = obtenCombinacion letrasCodificado letrasEspanol
            putStr "Combinación creada:                     "
            putStrLn combinacionNueva
            
            putStrLn "¿Reemplazar la combinación actual por esta? (s para sí)"
            c <- pideCaracter
            
            if c == 's' then do
              bucleMenu combinacionNueva mapa
            else do
              bucleMenu combinacion mapa
          
          -- Opción 5: mostramos la lista de posibilidades de una palabra.
          -- Es decir, mostramos las palabras del español que tengan la misma numeración que la palabra elegida
          -- Si hay más de 100 posibilidades, mostramos las 100 mejores
          '5' -> do
            mPalabrasNumeradasEspanol <- fuerzaMapa mapa
            putStr "Escribe una palabra: "
            palabra <- getLine
            let posibilidades = Map.findWithDefault [] (numeraPalabra palabra) mPalabrasNumeradasEspanol
            putStr (show (length posibilidades) ++ " palabras coincidentes")
            if length posibilidades > 100 then
              putStrLn "(mostrando las 100 mejores):"
            else
              putStrLn ":"
            print (take 100 posibilidades)
            
            bucleMenu combinacion (Just mPalabrasNumeradasEspanol)
          
          -- Desciframos automáticamente el texto probando las posibilidades de cada palabra
          -- Empezamos por las palabras con menos posibilidades y añadimos las posibilidades (es decir, los nuevos nodos)
          -- a la cola. Al elegir cada posibilidad, descartamos para cada palabra cifrada las palabras del español que ya
          -- no sean posibles. Tras esto, volvemos a elegir las palabras con menos posibilidades.
          --
          -- Para el texto codificado del ejemplo, este proceso dura unos 30 segundos si el programa está compilado (con -O2).
          -- Si no está compilado, tarda un rato más.
          --
          -- Dado que es posible que no todas las palabras se encuentren en el archivo de frecuencias, se ejecuta el algoritmo
          -- de descifrado con márgenes de error entre 0 y 5.
          --
          -- A modo de ejemplo, en el texto original hay una palabra que no se encuentra usando el archivo de frecuencias modificado.
          -- Si se usa el archivo de frecuencias original, el margen de error usado es mayor, puesto que las palabras con tildes se
          -- numeran de forma errónea. La solución clara habría sido reemplazar las tildes al leer del archivo, pero Haskell no es
          -- capaz de hacerlo con las funciones actuales por razones desconocidas (es decir, reconoce de forma distinta una 'á' en el
          -- archivo de frecuencias que la misma letra en este código, aun probando distintas codificaciones.
          --
          '6' -> do
            mPalabrasNumeradasEspanol <- fuerzaMapa mapa
            textoCodificado <- leeTextoCodificado
            let palabrasCodificado = words (soloLetras textoCodificado)
            
            -- Calculamos las posibilidades para cada palabra
            let posibilidades = map (\x -> (x, Map.findWithDefault [] (numeraPalabra x) mPalabrasNumeradasEspanol)) palabrasCodificado
            let posibilidadesOrdenadas = ordenaPor (length.snd) posibilidades
            let posibilidadesCombinaciones = map (\v -> map (\x -> obtenCombinacion (fst v) x) (snd v)) posibilidadesOrdenadas
          
            -- Buscamos la solucion. Permitimos hasta 5 errores.
            solucion <- buscaSolucionBucle posibilidadesCombinaciones 0 5
            
            -- Imprime la solución y el número de "nodos" explorados
            putStr "Solucion: "
            print solucion
            
            let combinacionSolucion = snd solucion
            putStrLn (usaCombinacion textoCodificado combinacionSolucion)
            
            bucleMenu combinacionSolucion (Just mPalabrasNumeradasEspanol)
        else do
          bucleMenu combinacion mapa
      return ()
    else do
      -- Si es un 0, terminamos
      return ()
  
  
--
-- El nodo contiene, en orden, la combinacion actual, el nivel de profundidad y la siguiente combinacion a probar,
-- además de las posibilidades de las siguientes palabras y el número de nodos explorados (aunque esto último es
-- posible quitarlo sin afectar al algoritmo).
--
data NodoProblema = NoSolucion | Nodo [Char] Int [Char] [[[Char]]] Int deriving Show

-- Busca una solución con el margen de error err. Si no la encuentra, prueba hasta llegar al margen de error maxErr.
buscaSolucionBucle posibilidadesCombinaciones err maxErr = do
 let nodoInicial = Nodo ['_' | i<-['a'..'z']] 0 ['_' | i<-['a'..'z']] posibilidadesCombinaciones 0
 putStrLn ("Buscando una solucion. Margen de error: " ++ (show err) ++ "/" ++ (show maxErr))
 let solucion = buscaSolucion [nodoInicial] 0 err
 if ((snd solucion) == "") && (err < maxErr) then do
  putStrLn "No se ha encontrado ninguna solucion. Probando con mayor margen de error."
  solucionRec <- buscaSolucionBucle posibilidadesCombinaciones (err+1) maxErr
  return solucionRec
 else
  return solucion

-- Función principal del algoritmo
-- Si no quedan posibilidades, no hemos encontrado solución
buscaSolucion [] ne maxErr = (ne,"")

buscaSolucion ((Nodo combinacion profundidad prueba posib err):xs) ne maxErr
 -- Si no hay posibilidades para 
 | prueba == [] = buscaSolucion ((siguientesNodos combinacion profundidad posib (err+1) maxErr) ++ xs) (ne+1) maxErr
 | (puedeUnirCombinaciones combinacion prueba) && (profundidad >= length posib) = (ne,uneCombinaciones combinacion prueba)
 | puedeUnirCombinaciones combinacion prueba = let nuevaComb = (uneCombinaciones combinacion prueba) in
  (buscaSolucion ((siguientesNodos nuevaComb profundidad posib err maxErr) ++ xs) (ne+1) maxErr)
 | otherwise = buscaSolucion xs (ne+1) maxErr

-- Actualizamos la lista de posibles combinaciones de cada palabra y ordenamos las palabras por su número de posibilidades
siguientesPC comb posib prof = [[] | i<-[1..prof]] ++ (ordenaPor length (map (\x -> filter (\y -> puedeUnirCombinaciones comb y) x) (drop prof posib)))


siguientesNodos comb prof posib err maxErr = let nuevaPosib = (siguientesPC comb posib prof) in
 let nodoErr = if err < maxErr then [Nodo comb (prof+1) [] nuevaPosib err] else [] in
  nodoErr ++ (map (\x -> (Nodo comb (prof+1) x nuevaPosib err)) (posib !! prof))
 
 
-- Obtiene la combinación que hay que usar para decodificar una palabra y obtener otra palabra como resultado
-- Las letras no usadas tienen una barra baja _
obtenCombinacion palabraCodificada palabra = map (\c -> if (elem c palabraCodificada) then (palabra !! (posicion c palabraCodificada)) else '_') ['a'..'z']


-- Une dos combinaciones
uneCombinaciones (x:xs) (y:ys)
 | x == '_' = y : (uneCombinaciones xs ys)
 | otherwise = x : (uneCombinaciones xs ys)
uneCombinaciones _ _ = []

-- Indica si es posible unir dos combinaciones (por ejemplo, si en una combinación la a es reemplazada por o, y en la otra por e, no es posible unirlas)
puedeUnirCombinaciones [] [] = True
puedeUnirCombinaciones (x:xs) (y:ys)
 | x == y = puedeUnirCombinaciones xs ys
 | x == '_' = (not (elem y xs)) && puedeUnirCombinaciones xs ys
 | y == '_' = (not (elem x ys)) && puedeUnirCombinaciones xs ys
 | otherwise = False
puedeUnirCombinaciones _ _ = False


-- Ordena una lista usando el valor devuelvo por la función dada al aplicarla a cada elemento
ordenaPor funcion lista = sortBy (\x y -> compare (funcion x) (funcion y)) lista

-- Numera una palabra. Si dos palabras tienen la misma numeración, entonces es posible encontrar una combinación para pasar
-- de una a otra. Por ejemplo, las palabras hada y como tienen la combinación 1232. Es posible pasar de la primera a la segunda
-- usando las transformaciones h->c, a->o y d->m
numeraPalabra xs = numeraPalabraRec xs '1'
numeraPalabraRec [] n = []
numeraPalabraRec (x:xs) n
  | x >= 'a' && x <= 'z' = n : (numeraPalabraRec (map (\c -> if c==x then n else c) xs) (succ n))
  | otherwise = x : (numeraPalabraRec xs n)

-- Descifra un texto usando una combinación
usaCombinacion [] combinacion = []
usaCombinacion (x:xs) combinacion
  | (x >= 'a' && x <= 'z') = (combinacion !! ((ord x) - (ord 'a'))) : usaCombinacion xs combinacion
  | (x >= 'A' && x <= 'Z') = (combinacion !! ((ord x) - (ord 'A'))) : usaCombinacion xs combinacion
  | otherwise = x : usaCombinacion xs combinacion

-- Obtiene la posición de un elemento dentro de una lista.
-- Se usa cuando se sabe que el elemento está en la lista
posicion x xs =
  case (elemIndex x xs) of
    Just p -> p
    Nothing -> -1

-- Transforma todo lo que no sean letras a-z en un texto a espacios
-- Utilizada para poder luego aplicar words sobre la lista y obtener así la lista de palabras
soloLetras [] = []
soloLetras (x:xs)
  | (x >= 'a' && x <= 'z') = x : soloLetras xs
  | otherwise = ' ' : (soloLetras xs)


-- Funcion para calcular las frecuencias del idioma,
-- a partir del fichero de frecuencias
calculaFrecuenciasEspanol :: IO [Int]
calculaFrecuenciasEspanol = do
  texto <- leeTextoFrecuencias
  
  -- Comenzamos a calcular a partir de la posicion 5
  let lineas = drop 5 (lines texto)
  
  -- Usamos las frecuencias de las primeras 10000 palabras
  let lineasParaFrecuencias = (take 10000 lineas)
  
  -- Cogemos la lista de palabras. Intentamos quitar las tildes (aunque por alguna razón no sirve)
  let listaPalabras = map reemplazaTildes (map ((!! 1).words) lineasParaFrecuencias)
  
  let listaFrecuenciasPalabra = map ((!! 2).words) lineasParaFrecuencias
  let listaFrecuenciasPalabra2 = map (\s -> read s::Int) listaFrecuenciasPalabra
  
  -- Ahora tenemos una lista de elementos (Palabra,Frecuencia)
  let listaPalabrasConFrecuencias = zip listaPalabras listaFrecuenciasPalabra2
  
  -- Para cada letra, la frecuencia es la suma de (frecuencia*apariciones)
  let abecedario = ['a'..'z']
  let frecuencias = map (\c -> sum ( map (\x -> (snd x) * length (  filter (c ==) (fst x)  ) ) listaPalabrasConFrecuencias )) abecedario
  return frecuencias
  

-- Numeramos todas las palabras del español
numeraPalabrasEspanol = do
  texto <- leeTextoFrecuencias
  let lineas = drop 5 (lines texto)
  return (map (\x -> let w = (words x)!!1 in (numeraPalabra w, [w])) lineas)
  

-- Devuelve una lista de las letras mas usadas,
-- a partir de la lista de frecuencias de las letras
listaLetrasPorFrecuencia frecuencias = map fst (ordenaPorFrecuencia (zip ['a'..'z'] frecuencias))

-- Ordena una lista por el segundo elemento de forma descendente.
ordenaPorFrecuencia xs = sortBy (\x y -> compare (snd y) (snd x)) xs
  
-- Funcion para calcular las frecuencias del texto original
-- Devuelve un vector de 26 posiciones indicando las frecuencias absolutas de las letras del abecedario
-- Por ejemplo, calculaFrecuenciasTexto !! 0 = Frecuencia absoluta de la letra a
calculaFrecuenciasTexto = do
  -- Leemos el texto codificado y lo convertimos a minusculas
  texto <- leeTextoCodificado
  let textoMinusculas = map toLower texto
  
  -- Calculamos las frecuencias
  let abecedario = ['a'..'z']
  let frecuencias = map (\c -> length (filter (c ==) textoMinusculas)) abecedario
  return frecuencias
 
 
-- Quita las tildes de un texto
-- Por alguna razón, no sirve al aplicarlo a la lectura del archivo de frecuencias
reemplazaTildes [] = []
reemplazaTildes (x:xs)
  | x == 'á' = 'a' : (reemplazaTildes xs)
  | x == 'é' = 'e' : (reemplazaTildes xs)
  | x == 'í' = 'i' : (reemplazaTildes xs)
  | x == 'ó' = 'o' : (reemplazaTildes xs)
  | x == 'ú' = 'u' : (reemplazaTildes xs)
  | x == 'ü' = 'u' : (reemplazaTildes xs)
  | otherwise = x : (reemplazaTildes xs)

-- Lee el texto codificado 
leeTextoCodificado = do
  texto <- readFile "texto.cod"
  return texto
  
-- Lee el texto del archivo de frecuencias
leeTextoFrecuencias = do
  texto <- readFile "frecuencias.txt"
  return texto