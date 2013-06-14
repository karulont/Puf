module Main where

import PufParser
import PufAST
import PufCompiler
import System.Environment
import Control.Monad
import System.FilePath
import Data.List

main :: IO ()
main = do { args <- getArgs
          ; case args of
              [] -> mainargs "--help"
              _  -> do {mapM mainargs args; return ()}
          }

mainargs :: String -> IO ()
mainargs "--version" = do { putStrLn "Puf parser & pretty printer ver 0.0.1"
                          ; putStrLn ""
                          }
mainargs "--help" = do { putStrLn "Puf parser & pretty printer"
                       ; putStrLn ""
                       ; putStrLn "Usage: Puf <filename>"
                       ; putStrLn "   <filename> - should be a Puf source code file"
                       }

mainargs x = do { input <- prepare x (takeDirectory x)
                ; putStr "Parsing file - "
                ; putStrLn x
                ; putStrLn ""
                ; let ast = parsePuf input
                ; putStrLn (show ast)
                ; putStrLn ""
                ; putStrLn (ppAST ast)
                ; putStrLn ""
                ; putStrLn "Compiling..."
                ; let compiled = compileAST ast
                ; writeFile (replaceExtension x "cbn") compiled
                ; putStrLn "Done"
                }

selfparse :: String -> IO Bool
selfparse x = do { input <- prepare x (takeDirectory x)
                 ; putStr "Parsing file - "
                 ; putStrLn x
                 ; putStrLn ""
                 ; pprinted <- return (ppAST (parsePuf input))
                 ; putStrLn pprinted
                 ; return (pprinted == ppAST (parsePuf pprinted))
                 }

prepare :: String -> String -> IO String
prepare x path = do { sample <- readFile x
                    ; (liftM unlines) (mapM (\a -> processLine a path) (lines sample))
                    }

processLine l path
  | isPrefixOf "#include" l = prepare (combine path ((words l) !! 1)) path
  | otherwise = return l
