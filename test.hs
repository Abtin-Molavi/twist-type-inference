module Main where

import System.IO
import System.Environment
import System.Exit
import TwistParsing

main = do args <- getArgs
          case args of
               arg:[] -> do handle <- openFile (head args) ReadMode
                            contents <- hGetContents handle
                            let tree' = TwistParsing.parse contents
                            case tree' of
                                 Left _ -> do putStrLn "Syntax error"
                                              exitFailure
                                 Right tree -> do putStrLn "Standard unparse:"
                                                  putStr (unparse tree)
                                                  putStrLn "\n\nVerbose unparse:"
                                                  putStr (unparseVerbose tree)
                                                  putStrLn ""
               _      -> do progName <- getProgName
                            putStrLn $ "Usage: " ++ progName ++ " filename.q"
                            exitFailure
