import System.Random
import Data.List
import Data.Maybe
import Data.List.Split (chunksOf)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM)
import Control.Monad (fmap)
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)

--Tipos definidos para representar respectivamente el tablero y las posiciones del mismo 
type Grid = [[Int]]
type Casilla = (Int, Int)

--PRE Recibe una lista y un elemento
usarNumero ::Eq a => a -> [a] -> [a]
--POST Devuelve la lista incial sin el elemento pasado como argumento
usarNumero n lista = delete n lista

--PRE Recibe una lista
elegir :: [a] -> IO a
--POST Devuelve un elemento aleatorio de esa lista
elegir lista = do
   let min = 0
       max = length lista - 1
   index <- randomRIO (min, max)
   return (lista !! index)

--PRE Recibe un Grid una Casilla y un entero
cambiarCasilla :: Grid -> Casilla -> Int -> Grid
--POST Devuelve el Grid cambiando el valor de la Casilla por el entero proporcionado
cambiarCasilla grid (fila, columna) numero = take fila grid
    ++ [take columna (grid !! fila) ++ [numero] ++ drop (columna + 1) (grid !! fila)]
    ++ drop (fila + 1) grid

--PRE Recibe un IO Grid una Casilla y un entero
cambiarCasillaIO :: IO Grid -> Casilla -> Int -> IO Grid
--POST Devuelve el IO Grid cambiando el valor de la Casilla por el entero proporcionado
cambiarCasillaIO sudoku (fila, columna) numero = do
  grid <- sudoku
  return $ take fila grid
    ++ [take columna (grid !! fila) ++ [numero] ++ drop (columna + 1) (grid !! fila)]
    ++ drop (fila + 1) grid

generarSudoku :: IO Grid
--POST Genera el sudoku inicial con todos los numeros inicializados a 0
generarSudoku = return $ chunksOf 9 $ replicate 81 0

--PRE Recibe una casilla
obtenerCuadrante :: Casilla -> Int
--POST Devuelve el numero del cuadrante al que pertenece dicha casilla
obtenerCuadrante (a,b) 
    |a < 3 && b < 3 = 0
    |a < 3 && b < 6 = 1
    |a < 3 = 2
    |a < 6 && b < 3 = 3
    |a < 6 && b < 6 = 4
    |a < 6 = 5
    |a < 9 && b < 3 = 6
    |a < 9 && b < 6 = 7
    |a < 9 = 8

--PRE Recibe una Casilla, un Grid y el entero que se quiere meter en dicha casilla
comprobarCuadrante :: Casilla -> Grid -> Int -> Bool
--POST Devuelve True si el entero no se encuentra en ese cuadrante y False en caso contrario
comprobarCuadrante casilla sudoku num =
       let nCuadrante = obtenerCuadrante casilla
           cuadrantes = concat (listaCuadrantes sudoku)
       in (not (num `elem` (cuadrantes !! nCuadrante)))

--PRE Recibe un Grid
listaCuadrantes :: Grid -> [Grid]
--POST Devuelve el Grid inicial pero en vez de separado por filas, separado por cuadrantes
listaCuadrantes sudoku = do
     let cuadrantes =  chunksOf 3 $ (chunksOf 3 $ concatMap (chunksOf 3) sudoku)
     let [p1, p2, p3] = cuadrantes
     let f1 = chunksOf 9 (concat $ ((concat . transpose) p1))
     let f2 = chunksOf 9 (concat $ ((concat . transpose) p2))
     let f3 = chunksOf 9 (concat $ ((concat . transpose) p3))
     return ((f1 ++ f2 ++ f3))

--PRE Recibe una Casilla, un Grid y el entero que se quiere meter en dicha casilla
comprobarFila :: Casilla -> Grid -> Int -> Bool
--POST Devuelve True si el entero no se encuentra en esa fila y False en caso contrario
comprobarFila (fila, _) sudoku num = not (num `elem` (sudoku !! fila))

--PRE Recibe una Casilla, un Grid y el entero que se quiere meter en dicha casilla
comprobarColumna :: Casilla -> Grid -> Int -> Bool
--POST Devuelve True si el entero no se encuentra en esa columna y False en caso contrario
comprobarColumna (_, columna) sudoku num = not (num `elem` [x | x <- map (!! columna) sudoku])

resolver :: IO Grid
-- POST Devuelve un IO Grid completamente resuelto 
resolver = do
  s <- sudokuInicial
  let sol = resolverSudoku s (0, 0) -- Llamada al método de resolución del sudoku desde la primera casilla
  case sol of
    Just grid -> do return grid
    Nothing -> resolver

-- PRE Recibe un Grid y la Casilla del sudoku en la que se quiere introducir un valor
resolverSudoku :: Grid -> Casilla -> Maybe Grid
-- POST Devuelve el sudoku resuelto o Nothing en caso de que no haya una solución posible
resolverSudoku sudoku (8, 9) = Just sudoku -- Caso de parada (si llegamos a este caso ya se ha resuelto el sudoku)
resolverSudoku sudoku (fila, 9) = resolverSudoku sudoku (fila+1, 0) -- Si estamos en la columna 9 y avanzamos una pasamos a la siguiente fila y reiniciamos el contador de columnas
resolverSudoku sudoku (fila, columna) = if  (sudoku !! fila !! columna /= 0) then resolverSudoku sudoku (fila, columna + 1)  -- Si la celda ya tiene un valor la ignoramos y pasamos a la siguiente
                                                      else if (posiblesValores == []) then Nothing -- Si no hay valores posibles, no se puede resolver el sudoku (caso error)
                                                      else buscarSolucion posiblesValores -- En otro caso buscamos las posibles soluciones mediante backtracking
                                                          where
                                                            -- Todos los posibles valores que puede tener la posición pasada como parámetro
                                                            posiblesValores = [x | x <- [1..9], comprobarFila (fila, columna) sudoku x, comprobarColumna (fila, columna) sudoku x, comprobarCuadrante (fila, columna) sudoku x] 
                                                            -- Método con backtrackin que resuelve el sudoku 
                                                            -- PRE Recibe una lista con los posibles valores a probar en la casilla
                                                            buscarSolucion :: [Int] -> Maybe Grid
                                                            -- POST Devuelve el sudoku resuelto o Nothing en caso de que no haya una solución posible
                                                            buscarSolucion [] = Nothing  -- Caso base, es decir, no se encuentra una solución válida para la celda actual
                                                            buscarSolucion (x:xs) = if (res == Nothing) then buscarSolucion xs -- Si no encuentra una solución probamos el siguiente valor de la lista
                                                                                                else res -- Si encuentra una solución la devolvemos
                                                                                                  where res = resolverSudoku (cambiarCasilla sudoku (fila, columna) x ) (fila, columna + 1) 


sudokuInicial :: IO Grid
-- POST Crea un IO Grid con los cuadrantes 0, 4 y 8 rellenos con números aleatorios (rellena toda la diagonal principal)
sudokuInicial = rellenarCuadrante (rellenarCuadrante (rellenarCuadrante (generarSudoku) 0) 4) 8

-- PRE Recibe dos enteros
posicionesCuadrante :: Int -> Int -> [Casilla]
--POST Devuelve la lista de todas las Casillas formadas por el producto cartesiano de los elementos pasados y los dos valores siguientes a cada uno 
posicionesCuadrante posMinFil posMinCol = [(x, y) | x <- [posMinFil .. posMinFil+2], y <- [posMinCol .. posMinCol+2]] -- Todas las posiciones del cuadrante
-- Por ejemplo la llamada "posicionesCuadrante 0 0" devolvería una lista con todas las posiciones del primer cuadrante y la llamada "posicionesCuadrante 6 3" del septimo

-- PRE Recibe un IO Grid y el numero del cuadrante a rellenar
rellenarCuadrante :: IO Grid -> Int -> IO Grid
-- POST Devuelve otro IO Grid con el cuadrante indicado relleno con números aleatoriamente
rellenarCuadrante sudoku cuadrante = cambiarValCuadrante sudoku (posicionesCuadrante (3*(cuadrante `div` 3)) (3*(cuadrante `mod` 3))) [1..9]
-- Como la división es entera cualquier valor de [0..2] div 3 devuelve un 0 (que lo usaremos como mínimo de las filas), [3..5] un 1 y [6..8] un 2.
-- El módulo del cuadrante entre 3 nos indica exactamente la columna en la que nos encontramos en cada caso.

-- PRE Recibe un IO Grid, una lista de Casillas y una lista de enteros
cambiarValCuadrante :: IO Grid -> [Casilla] -> [Int] -> IO Grid
-- POST Devuelve otro IO Grid con el valor de cada posición indicada en la lista de Casillas igual a uno de los elementos de la lista de enteros (Sin repetir dichos valores)
cambiarValCuadrante sudoku [] [] = sudoku -- Si no quedan ni posiciones ni valores por añadir acabamos
-- Mandamos como solución un nuevo sudoku con el numero elegido al azar introducido y eliminamos de la lista de posiciones la posicion rellenada y de la lista de numero el valor elegido
cambiarValCuadrante sudoku (posActual:resto) nums = do
   valor <- liftIO (elegir nums)
   cambiarValCuadrante (cambiarCasillaIO sudoku posActual valor) resto (usarNumero valor nums) 

-- PRE Recibe un entero y un Grid
quitarCasilla :: Int -> Grid -> Grid
--POST Devuelve un nuevo sudoku equivalente el pasado como parámetro pero con tantas casillas como indique el entero pasado como parámetro inicializadas a 0
quitarCasilla n sudoku = quitarCasillaAux n (fila, columna) sudoku -- Para hacer el método completamente aleatorio no pasamos como primera casilla a eliminar un valor fijo
        where 
                  fila = (sudoku !! 0 !! 0) - 1 -- Valor en la primera casilla del sudoku, se resta uno debido a que las casilla se numeran del 0 al 8
                  columna = (sudoku !! fila !! 0) - 1 -- Valor en la posición (fila, 0) con fila el valor calculado anteriormente

-- PRE Recibe un entero, una casilla y un Grid
quitarCasillaAux :: Int -> Casilla -> Grid -> Grid
-- POST Devuelve un Grid equivalente al pasado como parámetro pero con la casilla pasada como parámetro inicializada a 0
quitarCasillaAux 0 _ sudoku = sudoku -- Caso de parada, si el entero es 0 ya se han "vaciado" todas las casillas necesarias
quitarCasillaAux n (fila, columna) sudoku = do 
      let nuevoSudoku = cambiarCasilla sudoku (fila, columna) 0 -- Sudoku con la casilla indicanda como parametro con valor 0
      if (fila == 9) -- En caso de que la fila indicada no sea valida se pasa a la siguiente columna y se reinicia el contador de filas
          then quitarCasillaAux n (0, columna + 1) sudoku
      else if (columna == 9) -- En caso de que la columna no sea valida se vuelve a la primera posición del sudoku
          then quitarCasillaAux n (0, 0) sudoku
      else if (sudoku !! fila !! columna == 0) -- Si la casilla ya ha sido "vaciada" se intenta vaciar la casilla de abajo
          then quitarCasillaAux n (fila + 1, columna) sudoku
      else if (n `mod` 2 == 0) -- Si el entero pasado como parámetro es par se intenta eliminar la casilla (valor eliminado, columna)
           then quitarCasillaAux (n - 1) ((sudoku !! fila !! columna) - 1, columna) nuevoSudoku
      else -- Si el entero pasado como parámetro es impar se intenta eliminar la casilla (fila, valor eliminado)
           quitarCasillaAux (n - 1) (fila, (sudoku !! fila !! columna) - 1) nuevoSudoku

-- PRE Recibe un Grid
imprimirSudoku :: Grid -> IO ()
-- POST Imprime por consola el Grid pasado
imprimirSudoku sudoku = do
  putStr $ "\n       1     2     3      4     5     6      7     8     9\n" ++
    imprimirMatriz sudoku 0 1 ++ "\n" -- Imprimimos los números de las columnas y luego el resto del sudoku

-- PRE Recibe un Grid y dos contadores
imprimirMatriz :: Grid -> Int -> Int -> String
-- POST Imprime por consola el Grid pasado linea a linea
imprimirMatriz [] _ _= "    ---------------------------------------------------------\n" -- Línea final que indica el fin del sudoku
imprimirMatriz (x:xs) numero fila = if (numero `mod` 4 == 0) then  "    ---------------------------------------------------------\n\n" ++ imprimirMatriz (x:xs) 1 fila -- Caso para separar los cuadrantes 
                                                    else  (show fila) ++ "  " ++ "|" ++ imprimirLinea x 0  ++ "\n\n" ++ imprimirMatriz xs (numero + 1) (fila + 1) -- Caso estándar que imprime el número de fila y la respectiva fila

-- PRE Recibe un fila y un contador
imprimirLinea :: [Int] -> Int -> String
-- POST Imprime por consola la línea pasada
imprimirLinea [] _ = "|" -- Caso fin de línea
imprimirLinea (x:xs) numero = if (numero `mod` 3 == 0) then "|  " ++ mostrar x  ++ "  |" ++ imprimirLinea xs 1 -- Caso separar cuadrantes
                                                    else "  " ++ mostrar x  ++ "  |" ++ imprimirLinea xs (numero + 1) -- Caso estándar

-- PRE Recibe un entero
mostrar :: Int -> String
-- POST imprime una cadena vacía si recibe un 0 o, en otro caso, el número recibido
mostrar 0 = " "
mostrar n = show n

-- PRE Recibe un Grid, una Casilla y un Valor entero
comprobarValor :: Grid -> Casilla -> Int -> Bool
-- POST Devuelve True si el elemento del sudoku en la posición Casilla es igual al valor pasado o False en otro caso
comprobarValor sudoku (fila, columna) n = (sudoku !! fila !! columna) == n

-- PRE Recibe un Maybe Int
maybeToInt :: Maybe Int -> Int
-- POST Devuelve el entero pasado como parámetro sin su envoltorio en caso de que este esté entre 1 y 9 o 0 en cualquier otro caso
maybeToInt Nothing = 0
maybeToInt (Just n) = if (n < 1 || n > 9) then 0
                                                          else n

normasSudoku :: IO ()
-- POST Imprime por consola las normas del juego
normasSudoku = do
  putStrLn "Antes de comenzar a jugar se le pedirá que introduzca la dificultad deseada, para ello, introduzca el número indicado al lado de cada una de las opciones."
  putStrLn "A continuación se le mostrará una cuadrícula de 9 por 9 casillas separada en 9 sectores de 3 por 3 cada uno. \nEl objetivo del juego es rellenar todos los sectores con números del 1 al 9 siguiendo las siguientes restricciones:"
  putStrLn "No puede aparecer el mismo número dos veces en un mismo sector (recuadro 3 por 3 separado por las lineas dobles)."
  putStrLn "No puede aparecer el mismo número dos veces en una misma fila (de izquierda a derecha)."
  putStrLn "No puede aparecer el mismo número dos veces en una misma columna (de arriba a abajo)."
  putStrLn ""
  putStrLn "Para introducir los números deseados deberá introducir primero la fila donde desea poner el número, a continuación la columna, \ny finalmente el número que desee introducir."
  putStrLn "Una vez indicado el número a introducir pueden pasar dos cosas diferentes:"
  putStrLn "Que el número sea correcto y, por tanto, se introduzca dentro del sudoku y se muestre el sudoku actualizado."
  putStrLn "Que el número sea incorrecto, cosa que se le indicará con un mensaje de error y aumentando el contador de fallos. \nEn caso de llegar a 3 fallos se considerará que ha perdido y se le mostrará el sudoku resuelto."
  putStrLn ""

elegirDificultad :: IO ()
-- POST Recibe el nivel de dificultad del usuario, crea un sudoku y crea un segundo sudoku igual al primero con tantas posiciones eliminadas como indique la dificultad escogida
elegirDificultad = do
  putStrLn "Elija la dificultad del sudoku:"
  putStrLn ""
  putStrLn "Fácil: 1"
  putStrLn "Medio: 2"
  putStrLn "Difícil: 3"
  putStrLn ""
  numero <- getLine
  let numeroCasillas = case numero of
        "1" -> 46
        "2" -> 49
        "3" -> 53
        _  -> 0

  if (numeroCasillas == 0) then do -- Caso opción incorrecta
    putStrLn "Introduzca un número entre el 1 y el 3"
    elegirDificultad
  else do
    sudokuSolved <- resolver -- Creación sudoku resuelto
    let sudokuUnsolved = quitarCasilla numeroCasillas sudokuSolved -- Creación sudoku del usuario
    jugarSudoku sudokuUnsolved sudokuSolved numeroCasillas 0 -- Llamada al método para jugar al juego

volverAJugar :: IO () 
-- POST Crea un juego nuevo o sale del programa dependiendo de la respuesta del usuario
volverAJugar = do
  putStrLn "¿Quiere volver a jugar?\n1.Sí\n2.No"
  eleccion <- getLine
  if (eleccion == "1") then do
    elegirDificultad
  else do return ()

-- PRE Recibe un Grid sin resolver (con posiciones a 0), otro resuelto, el contador de casillas por rellenar para acabar y el contador de errores
jugarSudoku :: Grid -> Grid -> Int -> Int -> IO ()
-- POST Permite al usuario jugar guiandole e indicandole el estado del juego
jugarSudoku sudokuUnsolved sudokuSolved numCasillas numErrores = do
  if (numErrores == 3) then do -- Caso en el que el usuario ha perdido
    putStrLn ""
    putStrLn "------------------------------------------------------"
    putStrLn "Ha llegado al máximo de errores posibles, ha perdido."
    putStrLn ""
    putStrLn "La solución del sudoku era la siguiente:"
    imprimirSudoku sudokuSolved
    volverAJugar
  else if (numCasillas == 0) then do -- Caso en que el usuario ha ganado
    putStrLn "Felicidades, ha resuelto el sudoku."
    volverAJugar
  else do -- Caso jugada intermedia
    imprimirSudoku sudokuUnsolved
    putStrLn ""
    putStrLn $ "Errores: (" ++ show numErrores ++ " / 3)" -- Mostramos numero de errores
    putStrLn ""
    putStrLn "Introduzca la fila en la que desea poner un número:"
    filaStr <- getLine
    let fila = maybeToInt (readMaybe filaStr) -- Convertimos la selección en entero
    if (fila == 0) then do -- Caso fila incorrecta
      putStrLn "\x1b[31mLa fila introducida tiene que estar entre 1 y 9\x1b[0m"
      jugarSudoku sudokuUnsolved sudokuSolved numCasillas numErrores
    else do -- Caso fila correcta
        putStrLn "Introduzca la columna en la que desea poner un número:"
        columnaStr <- getLine
        let columna = maybeToInt (readMaybe columnaStr) -- Convertimos la selección en entero
        if (columna == 0) then do -- Caso columna incorrecta
          putStrLn "\x1b[31mLa columna introducida tiene que estar entre 1 y 9\x1b[0m"
          jugarSudoku sudokuUnsolved sudokuSolved numCasillas numErrores
        else if ((sudokuUnsolved !! (fila - 1) !! (columna - 1)) /= 0) then do -- Caso selección fila casilla ya rellenada
          putStrLn "\x1b[31mEsta casilla ya ha sido rellenada\x1b[0m"
          jugarSudoku sudokuUnsolved sudokuSolved numCasillas numErrores
        else do -- Caso Casilla correcta
          putStrLn "Introduzca el valor a probar:"
          valorStr <- getLine
          let valor = maybeToInt (readMaybe valorStr) -- Convertimos la selección en entero
          if (valor == 0) then do -- Caso valor incorrecto
            putStrLn "\x1b[31El valor introducido tiene que estar entre 1 y 9\x1b[0m"
            jugarSudoku sudokuUnsolved sudokuSolved numCasillas numErrores
          else do 
            if (comprobarValor sudokuSolved (fila - 1, columna - 1) valor) then do -- Caso valor introducido correcto
              putStrLn "\x1b[32mNúmero correcto\x1b[0m"
              jugarSudoku (cambiarCasilla sudokuUnsolved (fila - 1, columna - 1) valor) sudokuSolved (numCasillas - 1) numErrores
            else do -- Caso valor introducido incorrecto
              putStrLn "\x1b[31mERROR: número incorrecto \x1b[0m"
              jugarSudoku sudokuUnsolved sudokuSolved numCasillas (numErrores + 1)

main :: IO ()
-- POST Inicia la ejecución de una partida
main = do 
  putStrLn "--------------------------------"
  putStrLn "-            SUDOKU            -"
  putStrLn "--------------------------------"
  putStrLn "A continuación le explicaremos las normas del juego:"
  normasSudoku >> elegirDificultad