module ThajParser (parseIt, AST (..), ExpressionTree (..), splitListByElement) where

import Data.Maybe
import Data.List 
import Text.Read
-- English: Data type AST (Abstract Syntax Tree)
-- Español: Tipo de datos AST (Árbol de Syntaxis Abstracta)
data AST = EmptyAST 
        | ASTNode String [AST] 
        | ASTMath ExpressionTree
        deriving (Show)

data ExpressionTree = Times ExpressionTree ExpressionTree | Plus ExpressionTree ExpressionTree | Minus ExpressionTree ExpressionTree | DividedBy ExpressionTree ExpressionTree | Value Int | Var String deriving (Show)

-- English: builds abstract syntax tree from a list
-- Español: construye AST de una lista 
parseIt :: [String] -> Maybe AST
parseIt tokens = extractClass tokens 

-- English: extracts class name and adds it to the tree
-- Español: extrae nombre de clase y lo añade al árbol
extractClass :: [String] -> Maybe AST
extractClass [] = Nothing
extractClass (h1:t1)
                | h1 == "class" = case t1 of
                                        [] -> Nothing
                                        (h2:t2) -> case (extractFunctionBlocks (tail t2) 0 []) of -- (tail t2) -> we get rid of first {
                                                    Nothing -> Nothing
                                                    Just blocks -> Just (ASTNode h2 (map defineFunctions blocks)) 
                | otherwise = extractClass t1 

-- English: extracts function block
-- Español: extrae bloque de funcion
extractFunctionBlocks :: [String] -> Integer -> [String] -> Maybe [[String]]
extractFunctionBlocks (head:tail) level currentList 
                                | level == 0 = case head of
                                                    "{" -> extractFunctionBlocks tail (level + 1) (currentList ++ ["{"])
                                                    "}" -> Just []
                                                    str -> extractFunctionBlocks tail level (currentList ++ [str])
                                | level == 1 = case head of
                                                "}" -> Just ((currentList):(fromJust (extractFunctionBlocks tail (level - 1) [])))
                                                str -> extractFunctionBlocks tail level (currentList ++ [str])
                                | otherwise = extractFunctionBlocks tail level (currentList ++ [head]) 

-- English: Calls functions to add statements of current function to AST 
-- Español: Llama funciones que añade los declaraciones de nuestra funcion al AST
defineFunctions :: [String] -> AST
defineFunctions list = defineFunctionsAux list ""

-- English: Auxiliary function for adding functions to AST
-- Español: Funcion auxiliar para añadir funciones al AST
defineFunctionsAux :: [String] -> String -> AST
defineFunctionsAux [] _= EmptyAST
defineFunctionsAux (h1:t1) name
                    | h1 == "{" = ASTNode name (extractStatements (splitListByElement t1 ";"))
                    | otherwise = defineFunctionsAux t1 (name ++ " " ++ h1)

-- English: Divides list to multiple lists by splitter
-- Español: Divide la lista en listas múltiples con splitter 
splitListByElement :: Eq a => [a] -> a -> [[a]]
splitListByElement list splitter = splitListByElementAux list splitter []

-- English: Divides list to multiple lists by splitter
-- Español: Divide la lista en listas múltiples con splitter 
splitListByElementAux :: Eq a => [a] -> a -> [a] -> [[a]]
splitListByElementAux [] _ currentList = [currentList]
splitListByElementAux (head:tail) splitter currentList 
                                    | head == splitter = currentList:(splitListByElementAux tail splitter [])
                                    | otherwise        = splitListByElementAux tail splitter (currentList ++ [head])

-- English: Extracts statements from lists of strings (each list represents a statement) and returns a list of subrees
-- Español: Extrae los declaraciones de listas de strings  (cada lista representa una declaración) y devuelve la lista de subárboles
extractStatements :: [[String]] -> [AST]
extractStatements [] = []
extractStatements (head:tail) = (createTreeFromStatement head):(extractStatements tail)

-- English: Creates a tree from a list of strings that represents a statement
-- Español: Crea un árbol de lista de strings que representa una declaración
createTreeFromStatement :: [String] -> AST
createTreeFromStatement statement 
                        | printStatement statement = ASTNode "print" [ASTNode (head (getParamsForFunction False statement)) []]
                        | mathStatement statement  = case (buildExpressionTree (getInfix statement) Nothing) of
                                                        Nothing -> EmptyAST
                                                        Just tree -> ASTMath tree
                        | otherwise                = EmptyAST

-- English: Returns infix form of math expression 
-- Español: devuelve expresión matemática en forma infijo
getInfix :: [String] -> [String]
getInfix (h1:t1) 
            | elem h1 mathTypes = drop 2 t1
            | otherwise = t1

-- English: Builds expression tree if all is good syntactically or nothing if it's not
-- Español: Construye un árbol de expresion si la syntaxis está bien
buildExpressionTree :: [String] -> Maybe ExpressionTree -> Maybe ExpressionTree
buildExpressionTree [] tree = case tree of 
                                Nothing -> Nothing
                                _ -> tree
buildExpressionTree (h1:t1) tree = case h1 of
                                    "+" -> Just (Plus (fromJust tree) (fromJust (buildExpressionTree t1 Nothing)))
                                    "-" -> Just (Minus (fromJust tree) (fromJust (buildExpressionTree t1 Nothing)))
                                    "*" -> Just (Times (fromJust tree) (fromJust (buildExpressionTree t1 Nothing)))
                                    "/" -> Just (DividedBy (fromJust tree) (fromJust (buildExpressionTree t1 Nothing)))
                                    "(" -> let
                                            (subexpr, remainingExpr) = splitListsWithSameParanthesis 0 [] t1
                                           in
                                           buildExpressionTree remainingExpr (buildExpressionTree subexpr Nothing)
                                    _   -> let 
                                              maybeInt = readMaybe h1 :: Maybe Int
                                           in 
                                           case maybeInt of
                                               Just n -> buildExpressionTree t1 (Just (Value n))
                                               Nothing -> buildExpressionTree t1 (Just (Var h1))

-- English: 
-- Español: 
splitListsWithSameParanthesis :: Int -> [String] -> [String] -> ([String], [String])
splitListsWithSameParanthesis level first (h2:t2) 
                                | level == 0 && h2 == ")" = (first, t2)
                                | h2 == "(" = splitListsWithSameParanthesis (level + 1) (first ++ [h2]) t2
                                | h2 == ")" = splitListsWithSameParanthesis (level - 1) (first ++ [h2]) t2
                                | otherwise = splitListsWithSameParanthesis level (first ++ [h2]) t2
 
-- English: cheks if it's a print statement 
-- Español: comprueba si es una declaración que imprime algo
printStatement :: [String] -> Bool
printStatement list = elem "System" list && elem "out" list && elem "println" list

-- English: checks if it's a math statement
-- Español: comprueba si es una declaración matemática
mathStatement :: [String] -> Bool
mathStatement list = case list of
                        [] -> False
                        (head:tail) -> ((elem head mathTypes) || (head == "return")) && (foldl (\x y -> x || (elem y mathSymbols)) False tail)                        

-- English: list of math symbols in java 
-- Español: lista de símbolos matemáticos en Java
mathSymbols :: [String]
mathSymbols = ["+", "-", "*", "/"]

-- English: list of math types in java
-- Español: lista de tipos matemáticos en JAva
mathTypes :: [String]
mathTypes = ["int", "long", "short", "byte", "long", "double", "float"]

-- English: Get function that returns parameter values of function call
-- Español: Get funcion que devuleve valores de parametros de llamada a funcion
getParamsForFunction :: Bool -> [String] -> [String]
getParamsForFunction _ [] = []
getParamsForFunction alreadyFoundParanthesis (head:tail) 
                            | not (alreadyFoundParanthesis) = if head == "(" then getParamsForFunction True tail else getParamsForFunction alreadyFoundParanthesis tail 
                            | otherwise                     = case head of
                                                                ")"   -> []
                                                                ","   -> getParamsForFunction alreadyFoundParanthesis tail
                                                                param -> (param):(getParamsForFunction alreadyFoundParanthesis tail)  