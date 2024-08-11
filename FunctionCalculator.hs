module PF_Practica4 (Expresion(Var, Numero, Suma, Resta, Multiplicacion, Division, Logaritmo, Exponencial,Raiz,Seno,Coseno,Tangente), Sustitucion, deriv, simpl, queEs, getNumero, getVar, getExp1, getExp2, getMul, getPot, sust1) where
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import Text.Parsec.Prim (ParsecT)
import Control.Monad.Identity (Identity)
import Text.Read (readMaybe, parens, readListDefault)
import Text.Read (readPrec, readListPrec, readList)
import Text.ParserCombinators.ReadPrec
import Data.IORef
import Test.HUnit


-- Tipo de dato para representar cada funcion

data Expresion = Var Double String Double | -- a x b = ax^b
                        Numero Double | 
                        Suma Expresion Expresion | 
                        Resta Expresion Expresion |
                        Multiplicacion Expresion Expresion |
                        Division Expresion Expresion |
                        Logaritmo Expresion Expresion | -- Logaritmo de base y argumento
                        Exponencial Expresion Expresion | -- Base elevada a la potencia
                        Raiz Expresion Expresion | -- Raíz enésima del argumento
                        Seno Expresion |
                        Coseno Expresion |
                        Tangente Expresion deriving (Eq)

-- Tipo de dato para evaluar las funciones  

type Sustitucion = [(String, Double)]
 
--------------------PARSER----------------------
--Funcion que se salta los espacios encontrados
espacios :: Parser ()
espacios = skipMany (space <|> char ' ')

--Comprueba cual es el operador de la expresion
operador :: String -> Parser String
operador op = string op <* espacios

--Parsea las variables y los numeros
varNumParser :: Parser Expresion
varNumParser = do
  coef <- option 1.0 (try (do
    coef <- many1 digit
    char '.'
    coef' <- many1 digit
    return (read (coef ++ "." ++ coef')))
    <|> (many1 digit >>= return . read))
  name <- many1 letter <|> return ""
  espacios
  exponente <- option 1.0 (try (do
    coef2 <- many1 digit
    char '.'
    coef2' <- many1 digit
    return (read (coef2 ++ "." ++ coef2')))
    <|> (many1 digit >>= return . read))
  espacios
  return (if null name then Numero coef else Var coef name exponente)

--Parsea las expresiones compuestas
expresionParser :: ParsecT String () Identity Expresion
expresionParser = chainl1 terminoParser operadores

--Parsea cada termino individual de una expresion compuesta
terminoParser :: ParsecT String () Identity Expresion
terminoParser = entreParentesis <|> trigParser <|> varNumParser <|> expresionParser 

--Coge lo que esta dentro de parentesis
entreParentesis :: ParsecT String () Identity Expresion
entreParentesis = between (char '(' *> espacios) (char ')' *> espacios) expresionParser

--Identifica que tipo de Expresion binaria es cada termino
operadores :: ParsecT String () Identity (Expresion -> Expresion -> Expresion)
operadores = Text.Parsec.choice
  [ operador "*" >> return Multiplicacion
  , operador "/" >> return Division
  , operador "+" >> return Suma
  , operador "-" >> return Resta
  , operador "log" >> return Logaritmo
  , operador "^" >> return Exponencial
  , operador "sqrt" >> return Raiz
  ]

--Identifica que tipo de Expresion unaria es cada termino
trigParser :: ParsecT String () Identity Expresion
trigParser = Text.Parsec.choice
  [ sinParser
  , cosParser
  , tanParser
  ]

--Identifica la Expresion Seno
sinParser :: ParsecT String () Identity Expresion
sinParser = unaryOperator "sin" Seno

--Identifica la Expresion Coseno
cosParser :: ParsecT String () Identity Expresion
cosParser = unaryOperator "cos" Coseno

--Identifica la Expresion Tangente
tanParser :: ParsecT String () Identity Expresion
tanParser = unaryOperator "tan" Tangente

--Identifica la Expresion dentro de la Expresion unaria
unaryOperator :: String -> (Expresion -> Expresion) -> ParsecT String () Identity Expresion
unaryOperator opName opFunc = do
  _ <- operador opName
  exp <- terminoParser
  return (opFunc exp)

--Recibe la cadena inicial y la parsea usando las funciones auxiliares vistas anteriormente
parserExpresion :: String -> Either ParseError Expresion
parserExpresion input = parse (espacios *> expresionParser <* eof) "" input



-------------DERIVAR-------------------------------
--PRE: Recibe la expresion (Expresion) y una variable (String) sobre la que se quiere derivar
deriv :: Expresion -> String -> Expresion
--POST: Devuelve la expresion (Expresion) derivada
deriv (Numero _) variable = Numero 0
deriv (Var a x b) (y)
  | x == y = Var (a*b) x (b-1)
  | otherwise = Numero 0
deriv (Suma e1 e2) variable = 
   (Suma (deriv (e1) variable) (deriv (e2) variable)) 
deriv (Resta e1 e2) variable =
     (Resta (deriv (e1) variable) (deriv (e2)variable)) 
deriv (Multiplicacion e1 e2) variable =
    (Suma (Multiplicacion (deriv (e1) variable) e2)  (Multiplicacion (deriv (e2) variable) e1))
deriv (Division e1 e2) variable =
    Division (Resta (Multiplicacion (deriv e1 variable) e2) (Multiplicacion (deriv e2 variable) e1)) (Exponencial e2 (Numero 2))
deriv (Exponencial e1 e2) variable = Suma (Multiplicacion (Multiplicacion e2 (Exponencial e1 (Resta e2 (Numero 1)))) (deriv e1 variable))(Multiplicacion (Multiplicacion (deriv e2 variable) (Exponencial e1 e2)) (Logaritmo (Numero (exp 1.0)) e1)) --MAl
deriv (Logaritmo e1 e2) variable = Division (deriv e2 variable) (Multiplicacion (e2) (Logaritmo (Numero (exp 1.0)) e1))
deriv (Raiz e1 e2) variable = deriv (Exponencial (e2) (Division (Numero 1) e1)) variable --MAAL
deriv (Coseno e1) variable = Multiplicacion (Resta (Numero 0) (Seno e1)) (deriv e1 variable)
deriv (Seno e1) variable = Multiplicacion (Coseno e1) (deriv e1 variable)
deriv (Tangente e1) variable = Division (deriv e1 variable) (Exponencial (Coseno e1) (Numero 2))


---------------EVALUA----------------------------
--PRE: Recibe la expresion (Expresion) y una dupla (Sustitucion) de las variables que se quieren evaluar
sust1 :: Expresion -> Sustitucion -> Expresion
--POST: Devuelve la expresion (Expresion) evaluada
sust1 (Numero n) _ = (Numero n)
sust1 (Var a x b) sustitucion = 
    case lookup x sustitucion of
        Just v -> Numero (a*(v**b))
        Nothing -> (Var a x b) 
sust1 (Suma e1 e2) sustitucion = (Suma (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Resta e1 e2) sustitucion = (Resta (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Multiplicacion e1 e2) sustitucion = (Multiplicacion (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Division e1 e2) sustitucion = (Division (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Logaritmo e1 e2) sustitucion = (Logaritmo (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Exponencial e1 e2) sustitucion = (Exponencial (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Raiz e1 e2) sustitucion = (Raiz (sust1 e1 sustitucion) (sust1 e2 sustitucion))
sust1 (Seno e1) sustitucion = Seno (sust1 e1 sustitucion)
sust1 (Coseno e1) sustitucion = Coseno (sust1 e1 sustitucion)
sust1 (Tangente e1) sustitucion = Tangente (sust1 e1 sustitucion)


----------SIMPLIFICA-----------------
--PRE: Recibe una expresion (Expresion)
simpl :: Expresion -> Expresion
--POST: Devuelve la expresion simplificada
simpl (Numero n) = Numero n
simpl (Var a x b)  
    | b == 0 = Numero a
    | otherwise = Var a x b
simpl (Suma e1 e2)  
    | (simpl e1) == Numero 0 = (simpl e2) -- 0 + e2
    | (simpl e2) == Numero 0 = (simpl e1) -- e1 + 0
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e1))+(getNumero (simpl e2))) -- a + b
    | queEs (simpl e1) == 2 && queEs (simpl e2) == 2 && getVar (simpl e1) == getVar (simpl e2) && getPot (simpl e1) == getPot (simpl e2) = Var ((getMul (simpl e1)) + (getMul (simpl e2))) (getVar (simpl e1)) (getPot (simpl e1)) -- 2x + x
    | otherwise = Suma (simpl e1) (simpl e2)
simpl (Resta e1 e2)
    | (simpl e1) == (simpl e2) = Numero 0 -- a - a
    | (simpl e2) == Numero 0 = (simpl e1) -- a - 0
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e1))-(getNumero (simpl e2))) -- a - b
    | queEs (simpl e1) == 2 && queEs (simpl e2) == 2 && getVar (simpl e1) == getVar (simpl e2) && getPot (simpl e1) == getPot (simpl e2) = Var ((getMul (simpl e1)) - (getMul (simpl e2))) (getVar (simpl e1)) (getPot (simpl e1)) -- 2x - x
    | otherwise = Resta (simpl e1) (simpl e2)
simpl (Multiplicacion e1 e2)
    | (simpl e1) == Numero 0 || (simpl e2) == Numero 0 = Numero 0
    | (simpl e1) == Numero 1 = (simpl e2)
    | (simpl e2) == Numero 1 = (simpl e1)
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e1))*(getNumero (simpl e2))) -- a * b
    | queEs (simpl e1) == 2 && queEs (simpl e2) == 2 && getVar (simpl e1) == getVar (simpl e2) = Var ((getMul (simpl e1)) * (getMul (simpl e2))) (getVar (simpl e1)) ((getPot (simpl e1)) + (getPot (simpl e2))) -- 3x * 2x^3
    | otherwise = Multiplicacion (simpl e1) (simpl e2)
simpl (Division e1 e2)
    | (simpl e1) == Numero 0 = Numero 0 -- 0/e2
    | (simpl e2) == Numero 1 = simpl(e1) -- e1/1
    | (simpl e1) == (simpl e2) = Numero 1 -- e1/e1
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e1))/(getNumero (simpl e2))) -- a/b
    | queEs (simpl e1) == 2 && queEs (simpl e2) == 2 && getVar (simpl e1) == getVar (simpl e2) = Var ((getMul (simpl e1)) / (getMul (simpl e2))) (getVar (simpl e1)) ((getPot (simpl e1)) - (getPot (simpl e2))) --x^2/x^3
    | otherwise = Division (simpl e1) (simpl e2)
simpl (Exponencial e1 e2)
    | (simpl e1) == Numero 0 = Numero 0 -- 0^e2
    | (simpl e2) == Numero 0 = Numero 1 -- e1^0
    | (simpl e2) == Numero 1 = (simpl e1) --e1^1
    | queEs (simpl e1) == 2 && queEs (simpl e2) == 1 = Var (getMul (simpl e1)) (getVar (simpl e1)) ((getPot (simpl e1)) * (getNumero (simpl e2)))   --(ax^b) ^c
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e1))**(getNumero (simpl e2))) --a^b
    | otherwise = Exponencial (simpl e1) (simpl e2)
simpl (Raiz e1 e2)
    | (simpl e1) == Numero 1 = (simpl e2) 
    | (simpl e2) == Numero 0 = Numero 0 -- raiz de 0
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero ((getNumero (simpl e2))**(1/(getNumero (simpl e1)))) -- raiz de un numero
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 2 && (getPot (simpl e2)) == (getNumero (simpl e1)) = (Var ((getMul (simpl e2))**1/(getNumero (simpl e1))) (getVar (simpl e2)) 1) --raiz de x^2 es x
    --  NUMERO                    VAR                     POT>NUM
    | otherwise = Raiz (simpl e1) (simpl e2)
simpl (Tangente e1)
    | queEs (simpl e1) == 1 = Numero (tan (getNumero (simpl e1)))
    | otherwise = Tangente (simpl e1) 
simpl (Seno e1)
    | queEs (simpl e1) == 1 = Numero (sin (getNumero (simpl e1)))
    | otherwise = Seno (simpl e1)
simpl (Coseno e1)
    | queEs (simpl e1) == 1 = Numero (cos (getNumero (simpl e1)))
    | otherwise = Coseno (simpl e1)
simpl (Logaritmo e1 e2)
    | queEs (simpl e1) == 1 && queEs (simpl e2) == 1 = Numero (logBase (getNumero (simpl e1)) (getNumero (simpl e2)))
    | otherwise = Logaritmo (simpl e1) (simpl e2)
    
--PRE: Recibe la funcion que se quiere aplicar, la expresion y la dupla (Sustitucion) en caso de querer evaluar o la variable (String)
-- en caso de querer derivar
simpl2 :: (Expresion -> t -> Expresion) -> Expresion -> t -> Expresion
--POST: Devuelve la expresion simplificada despues de aplicarle la funcion
simpl2 fun exp sus = simpl (fun exp sus)

---------------AUXILIARES-----------------------------------

--PRE: Recibe una expresion (Expresion)
queEs :: Expresion -> Int
--POST: Devuelve que tipo de expresion es
queEs (Numero _) = 1
queEs (Var _ _ _) = 2
queEs (Suma _ _) = 3
queEs (Resta _ _) = 4
queEs (Multiplicacion _ _) = 5
queEs (Division _ _) = 6
queEs (Logaritmo _ _) = 7
queEs (Exponencial _ _) = 8
queEs (Raiz _ _) = 9
queEs (Seno _) = 10
queEs (Coseno _) = 11
queEs (Tangente _) = 12

--PRE: Recibe una expresion (Expresion) de tipo Numero
getNumero :: Expresion -> Double
--POST: Devuelve el valor de Numero
getNumero (Numero n) = n

----PRE: Recibe una expresion (Expresion) de tipo Var
getVar :: Expresion -> String
--POST: Devuelve el string de Var
getVar (Var a e1 b) = e1

----PRE: Recibe una expresion (Expresion) de tipo Var
getMul :: Expresion -> Double
--POST: Devuelve el coeficiente de Var
getMul (Var a _ _) = a

----PRE: Recibe una expresion (Expresion) de tipo Var--Devuelve el grado de la variable
getPot :: Expresion -> Double
--POST: Devuelve el exponente de Var
getPot (Var _ _ b) = b

----PRE: Recibe una expresion (Expresion) de tipo Var
getExp1 :: Expresion -> Expresion
--POST: Devuelve le base de la Exponencial
getExp1 (Exponencial e1 _) = e1

----PRE: Recibe una expresion (Expresion) de tipo Exponencial
getExp2 :: Expresion -> Expresion
--POST: Devuelve el exponente de la Exponencial
getExp2 (Exponencial _ e2) = e2

----PRE: Recibe una expresion (Expresion)
listaVar :: Expresion -> [String]
--POST: Devuelve una lista con los String de las Var que aparecen
listaVar (Numero n) = []
listaVar (Var a x b) = [x]
listaVar (Suma e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Resta e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Multiplicacion e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Division e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Logaritmo e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Exponencial e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Raiz e1 e2) = (listaVar e1) ++ (listaVar e2)
listaVar (Seno e1) = (listaVar e1)
listaVar (Coseno e1) = (listaVar e1)
listaVar (Tangente e1) = (listaVar e1)

-------------------- TESTS -----------------------------------------
testDeriv :: Test
testDeriv = TestList
   [TestLabel "1" (TestCase $ assertEqual "1" (Suma (Coseno (Var 1 "x" 1)) (Resta (Numero 0) (Seno (Var 1 "x" 1)))) ( simpl2 deriv (Suma (Seno (Var 1 "x" 1)) (Coseno (Var 1 "x" 1))) "x"))
   ,TestLabel "2" (TestCase $ assertEqual "2" (Division (Multiplicacion (Var 3 "x" 2) (Numero 3)) (Numero 9)) (simpl2 deriv (Division (Suma (Var 1 "x" 3) (Numero 2)) (Numero 3)) "x"))
   ,TestLabel "3" (TestCase $ assertEqual "3" (Multiplicacion (Numero (1/2)) (Var 1 "x" (-1/2))) (simpl2 deriv (Raiz (Numero 2) (Var 1 "x" 1)) "x"))
   ,TestLabel "4" (TestCase $ assertEqual "4" (Multiplicacion (Multiplicacion (Multiplicacion (Numero 0.5) (Var 1.0 "x" (-0.5))) (Exponencial (Numero 10.0) (Raiz (Numero 2.0) (Var 1.0 "x" 1.0)))) (Numero 2.302585092994046)) (simpl2 deriv (Exponencial (Numero 10) (Raiz (Numero 2) (Var 1 "x" 1))) "x") )
   ,TestLabel "5" (TestCase $ assertEqual "5" (Suma (Division (Numero 1) (Exponencial (Coseno (Var 1 "x" 1)) (Numero 2))) (Division (Var 4 "x" 1) (Multiplicacion (Var 2 "x" 2) (Numero 2.302585092994046)))) (simpl2 deriv (Suma (Tangente (Var 1 "x" 1)) (Logaritmo (Numero 10) (Var 2 "x" 2))) "x"))
   ]

testEval :: Test
testEval = TestList
   [TestLabel "1" (TestCase $ assertEqual "1" (Numero 1) (simpl2 sust1 (Suma (Seno (Var 1 "x" 1)) (Coseno (Var 1 "x" 1))) [("x", 0)]))
   ,TestLabel "2" (TestCase $ assertEqual "2" (Division (Suma (Numero 12) (Var 4 "y" 5)) (Numero 16) ) (simpl2 sust1 (Division (Suma (Var 3 "x" 2) (Var 4 "y" 5)) (Multiplicacion (Var 2 "x" 1) (Var 4 "z" 5))) [("x", 2), ("z", 1)]))
   ,TestLabel "3" (TestCase $ assertEqual "3" (Numero 6) (simpl2 sust1 (Suma (Raiz (Numero 2) (Var 2 "x" 1)) (Exponencial (Numero 2) (Var 1 "x" 1))) [("x", 2)] ))
   ,TestLabel "4" (TestCase $ assertEqual "4" (Numero 2) (simpl2 sust1 (Division (Resta (Var 3 "x" 2) (Var 2 "y" 1)) (Suma (Var 2 "x" 1) (Var 1 "y" 2))) [("x", 2), ("y", 1)])) 
   ,TestLabel "5" (TestCase $ assertEqual "5" (Numero (-2.757213324942802)) (simpl2 sust1 (Multiplicacion (Tangente ((Var 1 "x" 1))) (Logaritmo (Var 1 "y" 1) ((Var 1 "x" 2)))) [("x", 2), ("y", 3)]))
   ]

testParser :: Test
testParser = TestList
   [TestLabel "1" (TestCase $ assertEqual "1" (Right (Suma (Suma (Var 3.0 "x" 1.0) (Multiplicacion (Var 2.0 "y" 1.0) (Coseno (Var 2.0 "x" 1.0)))) (Raiz (Numero 2.0) (Var 1.0 "x" 1.0)))) (parserExpresion "3x + (2y * cos(2x)) + (2 sqrt x)"))
   ,TestLabel "2" (TestCase $ assertEqual "2" (Right (Division (Seno (Suma (Var 3.0 "x" 1.0) (Numero 2.0))) (Logaritmo (Numero 10.0) (Var 1.0 "x" 1.0)))) (parserExpresion "sin(3x + 2) / (10 log x)"))
   ,TestLabel "3" (TestCase $ assertEqual "3" (Right (Resta (Suma (Tangente (Var 4.0 "y" 1.0)) (Var 2.0 "x" 4.0)) (Exponencial (Var 3.0 "x" 1.0) (Coseno (Var 1.0 "x" 1.0))))) (parserExpresion "tan(4y) + 2x4 - (3x^cos(x))"))
   ,TestLabel "4" (TestCase $ assertEqual "4" (Right (Suma (Var 1.0 "z" 1.0) (Division (Numero 3.0) (Multiplicacion (Var 1.0 "x" 1.0) (Var 1.0 "y" 1.0))))) (parserExpresion "z + (3/(x*y))"))
   ,TestLabel "5" (TestCase $ assertEqual "5" (Right (Multiplicacion (Seno (Coseno (Var 1.0 "x" 1.0))) (Tangente (Numero 3.0)))) (parserExpresion "sin(cos(x)) * tan(3)"))
   ]

instance Show Expresion where
  show (Numero n) = show n
  show (Var a x b) 
    | a == 1 && b/=1 = tail (init (show x)) ++ "^" ++ show b
    | a == 1 && b==1 = tail (init (show x))
    | a /= 1 && b==1 = show a ++ tail (init (show x))
    |otherwise =  show a ++ tail (init (show x)) ++ "^" ++ show b
  show (Suma e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
  show (Resta e1 e2) 
   | e1 /= Numero 0 = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
   | otherwise = "(" ++ "- " ++ show e2 ++ ")"
  show (Multiplicacion e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Division e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
  show (Logaritmo e1 e2)
    | e1 == Numero (exp 1.0) = "ln(" ++ show e2 ++ ")"
    | e1 == Numero 10 = "log(" ++ show e2 ++ ")"
    | otherwise = "log(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Exponencial e1 e2) = "(" ++ show e1 ++ " ^ " ++ show e2 ++ ")"
  show (Raiz e1 e2) = "sqrt(" ++ show e1 ++ ", " ++ show e2 ++ ")"
  show (Seno e) = "sin(" ++ show e ++ ")"
  show (Coseno e) = "cos(" ++ show e ++ ")"
  show (Tangente e) = "tan(" ++ show e ++ ")"

--Funcion auxliar del main
evaluar :: IORef [(String, Double)] -> Expresion -> IO ()
evaluar susti exp = do
    listaOriginal <- readIORef susti
    let lista = [x | x <- nub (listaVar exp), not (any (\(s, _) -> s == x) listaOriginal ) ]
    putStrLn "Escriba sobre qué variable quiere evaluar la función. " 
    putStrLn $ "Variables disponibles: " ++ show lista
    s <- getLine
    if not (s `elem` lista) 
      then do
        putStrLn "\x1b[31mVariable no válida\x1b[0m"
        putStrLn "¿Desea evaluar otra variable? (s/n)"
        input <- getLine
        if input == "s"
            then evaluar susti exp
            else do
                res <- readIORef susti >>= \s -> pure (simpl2 sust1 exp s)
                putStrLn $ "La función sustituida es: " ++ show res
      else do
        putStrLn "Escriba por qué número quiere sustituirla:"
        n <- getLine
        let num = read n
        let nuevaLista = (s, num) : listaOriginal
        writeIORef susti nuevaLista
        putStrLn "¿Desea evaluar otra variable? (s/n)"
        input <- getLine
        if input == "s"
            then evaluar susti exp
            else do
                res <- readIORef susti >>= \s -> pure (simpl2 sust1 exp s)
                putStrLn $ "La función evaluada es: " ++ show res

main :: IO ()
main = do
  -- Simplificar se comprueba en los tests de derivar y evaluar ya que devuelven la funcion simplificada
  putStrLn "Tests de Derivar:"
  runTestTT testDeriv
  putStrLn "Tests de Evaluar:"
  runTestTT testEval
  putStrLn "Tests del Parser:"
  runTestTT testParser
  putStrLn "--------------------------------"
  putStrLn "-   CALCULADORA DE FUNCIONES   -"
  putStrLn "--------------------------------"
  putStrLn "Derivar: 1"
  putStrLn "Simplificar: 2"
  putStrLn "Evaluar: 3"
  putStrLn ""
  putStrLn "Elija el numero de operación que quiera realizar:"
  numero <- getLine
  if numero `elem` ["1", "2", "3"] 
    then do
      putStrLn ""
      putStrLn "FORMATO DE LAS EXPRESIONES:"
      putStrLn "--------------------------------------------------------------------------"
      putStrLn "Variables: axb para a*x^b (a y b son numeros, si son 1 se pueden omitir)."
      putStrLn "Suma: e1 + e2"
      putStrLn "Resta: e1 - e2"
      putStrLn "Multiplicacion: e1 * e2"
      putStrLn "Division: e1 / e2"
      putStrLn "Logaritmo: (e1 log e2) (e1 es la base y e2 el argumento) **Paréntesis obligatorios**"
      putStrLn "Exponencial: (e1 ^ e2) **Paréntesis obligatorios**"
      putStrLn "Raiz: (e1 sqrt e2) (e1 es el indice y e2 el argumento) **Paréntesis obligatorios**"
      putStrLn "Seno: sin (e1) **Paréntesis obligatorios**"
      putStrLn "Coseno: cos (e1) **Paréntesis obligatorios**"
      putStrLn "Tangente: tan (e1) **Paréntesis obligatorios**"
      putStrLn ""
      putStrLn "Dentro de cada expresion se pueden meter otras expresiones"
      putStrLn "Para que se siga el orden de las operaciones hay que usar paréntesis"
      putStrLn "----------------------------------------------------------------------------"
      putStrLn ""
      putStrLn "Ingrese una expresión:"
      input <- getLine
      
      let op = parserExpresion input
      
      case op of
        Left err -> do 
          putStrLn $ "\x1b[31mError de análisis: \x1b[0m" ++ show err
          main
        Right expr -> do
            putStrLn $ "Expresión analizada correctamente: " ++ show expr
            let exp = expr
            case numero of
                "1" -> do
                    putStrLn ""
                    putStrLn "Escriba sobre qué variable quiere derivar"
                    variable <- getLine
                    let res = simpl2 deriv exp variable
                    putStrLn ""
                    putStrLn $ "La derivada es: " ++ (show res)
                    if ((listaVar res) /= [])
                       then do
                            putStrLn "¿Desea evaluar la función derivada resultante? (s/n)"
                            ev <- getLine
                            case ev of
                              "s" -> do 
                                susti <- newIORef []
                                evaluar susti res
                              _ -> return()
                        else return()
                "2" -> do
                    putStrLn ""
                    let res = simpl exp
                    putStrLn $ "La función simplificada es: " ++ (show res)
                "3" -> do
                    susti <- newIORef []
                    evaluar susti exp 
            putStrLn "¿Quiere hacer alguna operación más? (s/n)"
            i <- getLine
            case i of
                "s" -> main
                _ -> return ()
    else do
      putStrLn "\x1b[31mOpción no válida\x1b[0m"
      putStrLn "¿Quiere hacer alguna operación más? Si/No"
      i <- getLine
      case i of
            "Si" -> main
            "si" -> main
            _ -> return ()
              
        
