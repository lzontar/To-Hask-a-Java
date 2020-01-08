module ThajLexer (lexicalAnalysis) where

import Data.Bool
import Data.Maybe
-- English: Function that does the lexical analysis (extracts tokens from the program and builds a list of these tokens)
-- Español: Funcion que hace análisis léxico (extrae tokens del programa y construye una lista usando estos tokens)
lexicalAnalysis :: String -> Maybe [String]
lexicalAnalysis code = tokenizeStrings (foldl (\list word -> list ++ (splitOnJavaPunctuations word)) [] (words code))

-- English: Checks if character is Java punctuation 
-- Español: Comprueba si el carácter es la puntuación de Java
isJavaPunctuation :: Char -> Bool
isJavaPunctuation c = elem c allJavaPunctuations 

-- English: List of Java punctuations
-- Español: Lista de puntuaciones de Java
allJavaPunctuations :: [Char]
allJavaPunctuations = ['.', ',', ':', ';', '{', '}', '[', ']', '(', ')', '"', '\'']

-- English: Splits string to a list of strings on Java punctuations
-- Español: Divide string en una lista de strings basado en puntuaciones de Java
splitOnJavaPunctuations :: String -> [String]
splitOnJavaPunctuations word = splitOnJavaPunctuationsAux word "" []

-- English: Auxiliary function of splitOnJavaPunctuations
-- Español: Funcion auxiliar de splitOnJavaPunctuations
splitOnJavaPunctuationsAux :: String -> String -> [String] -> [String]
splitOnJavaPunctuationsAux "" "" list = list
splitOnJavaPunctuationsAux "" lastSubword list = list ++ [lastSubword]
splitOnJavaPunctuationsAux (headOfWord:tailOfWord) constructedWord list 
                                | constructedWord == "" && (isJavaPunctuation headOfWord) = splitOnJavaPunctuationsAux tailOfWord "" (list ++ [[headOfWord]])
                                | (isJavaPunctuation headOfWord) = splitOnJavaPunctuationsAux tailOfWord "" (list ++ [constructedWord, [headOfWord]])
                                | otherwise = splitOnJavaPunctuationsAux tailOfWord (constructedWord ++ [headOfWord]) list

-- English: Maybe returns a list of proper tokens
-- Español: Quizas devuelve una lista de tokens fichas adecuadas
tokenizeStrings :: [String] -> Maybe [String]
tokenizeStrings list = tokenizeStringsAux list "" False

-- English: Auxiliary function of tokenizeStrings
-- Español: Funcion auxiliar de tokenizeStrings
tokenizeStringsAux :: [String] -> String -> Bool -> Maybe [String]
tokenizeStringsAux [] _ True = Nothing
tokenizeStringsAux [] _ False = Just []
tokenizeStringsAux (head:tail) currentString isStringBuilding 
                            | head == "\"" && isStringBuilding = Just ((currentString ++ head):(fromJust (tokenizeStringsAux tail "" False)))
                            | head == "\"" && (not (isStringBuilding)) = tokenizeStringsAux tail head True
                            | isStringBuilding = tokenizeStringsAux tail (currentString ++ head) isStringBuilding
                            | otherwise = Just (head:(fromJust (tokenizeStringsAux tail currentString isStringBuilding)))