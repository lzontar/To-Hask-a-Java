module ThajGenerator (convertASTToHaskell) where

import ThajParser (AST (..), ExpressionTree (..), splitListByElement)
import Data.List
import Data.Maybe

-- English: Main function of code generator which returns IO() that writes code to .hs file
-- Español: La funcion principal de generador de código que devuelve IO(), que escribe código en .hs fichero
convertASTToHaskell :: AST -> Integer-> IO()
convertASTToHaskell tree level = case tree of
                                    EmptyAST          -> print "Something went wrong... Sorry :("
                                    ASTNode name functions -> let
                                                                functionStringList = (map generateFunctionString functions)  
                                                              in 
                                                              if elem Nothing functionStringList then
                                                                  print "Something went wrong. You should check syntax of your statements! ;)"
                                                              else 
                                                                  writeFile (name ++ (".hs"))  ("module " ++ name ++ " where\n" ++ (intercalate "\n" (map fromJust functionStringList)) ++ "\n")

-- English: Generates a string for each function in Java file
-- Español: Genera el String para cada funcion en fichero Java                                
generateFunctionString :: AST -> Maybe String
generateFunctionString EmptyAST = Just "\n"
generateFunctionString (ASTNode functionDefinition statements) = let 
                                                                    statementStringList  = (map generateStatementString statements)
                                                                 in
                                                                 if elem Nothing statementStringList then
                                                                    Nothing
                                                                 else 
                                                                     Just ((getFunctionNameFromDefinition functionDefinition) ++ (getParamsString functionDefinition) ++ " = " ++ (intercalate "\n" (map fromJust statementStringList)) ++ "\n")

-- English: Get function that returns parameter names from function definition
-- Español: Get funcion que devuelve nombres de parametros de definición de funcion
getParamsString :: String -> String
getParamsString def = foldl (\x y -> x ++ " " ++ y) "" (map (last) (splitListByElement (extractParams (words def) [] False) ","))

-- English: Extracts parameters from function definition
-- Español: Extrae los parametros de definición de funcion
extractParams :: [String] -> [String] -> Bool -> [String]
extractParams [] paramList _ = paramList
extractParams (head:tail) paramList True 
                | head == ")" = extractParams tail paramList False
                | otherwise   = extractParams tail (paramList ++ [head]) True
extractParams (head:tail) paramList False 
                | head == "(" = extractParams tail paramList True
                | otherwise   = extractParams tail paramList False

-- English: Returns function name 
-- Español: Devulve nombre de funcion
getFunctionNameFromDefinition :: String -> String
getFunctionNameFromDefinition definition = head (filter (\x -> (isNotAccessModifier x) && (isNotNonAccessModifier x) && (isNotReturnType x)) (words definition))

-- English: checks if string is not access modifier
-- Español: comprueba si string no es access modifier
isNotAccessModifier :: String -> Bool
isNotAccessModifier s = not (elem s ["public", "private", "protected"])

-- English: checks if string is not non-access modifier
-- Español: comprueba si string no es non-access modifier
isNotNonAccessModifier :: String -> Bool
isNotNonAccessModifier s = not (elem s ["static", "final", "abstract", "synchronized"])

-- English: checks if string is not return type
-- Español: comprueba si string no es tipo que funcion devuelve
isNotReturnType :: String -> Bool
isNotReturnType s = not (elem s ["void", "int", "char", "boolean", "float", "double", "byte", "short", "long", "String"])

-- English: generates string for each statement 
-- Español: genera string por cada declaración
generateStatementString :: AST -> Maybe String
generateStatementString EmptyAST = Just "\n"
generateStatementString (ASTNode statementKey statement) = case statementKey of
                                                            "print" -> case statement of
                                                                        [ASTNode stringToPrint _] -> Just ("print " ++ stringToPrint ++ "\n")
                                                                        _               -> Nothing
                                                            _       -> Nothing
generateStatementString (ASTMath expressionTree) = Just (generateMathExpression expressionTree)

-- English: generates string that describes math expression
-- Español: genera string de expresión matemática
generateMathExpression :: ExpressionTree -> String
generateMathExpression expr = case expr of 
                                Times left right     -> "(" ++ (generateMathExpression left) ++ "*" ++ (generateMathExpression right) ++ ")"
                                DividedBy left right -> "(" ++ (generateMathExpression left) ++ "/" ++ (generateMathExpression right) ++ ")"
                                Plus left right      -> "(" ++ (generateMathExpression left) ++ "+" ++ (generateMathExpression right) ++ ")"
                                Minus left right     -> "(" ++ (generateMathExpression left) ++ "-" ++ (generateMathExpression right) ++ ")"
                                Value n              -> show n
                                Var x                -> x