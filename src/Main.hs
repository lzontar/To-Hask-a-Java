module Main (main, thaj) where

import System.IO
import ThajLexer (lexicalAnalysis)
import ThajParser (parseIt)
import ThajGenerator (convertASTToHaskell)

-- English: Main function to test if compiler is runnable
-- Español: Main funcion para probar si podemos ejecutar compilador
main = putStrLn "Thanks for using To-Hask-a-Java compiler!"

-- English: Function that calls all the steps that compiler takes while compiling (lexical analysis, parsing, code generation)
-- Español: Funcion que llama todos los pasos que compilador hace durante compilación (analisis léxico, parsing, generación de código)
thaj :: String -> IO()
thaj file = do
    code <- readFile file
    case (lexicalAnalysis code) of
        Nothing -> print "There has been an error while trying to tokenize input Java code. Check your syntax! :)"
        Just tokens -> case (parseIt tokens) of
                            Nothing -> print "There has been an error while trying to parse the tokens of input Java code. Check your Syntax! :)"
                            Just tree -> convertASTToHaskell tree 0
