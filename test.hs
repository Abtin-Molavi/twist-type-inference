module Main where

import System.Environment
import System.Exit
import System.IO
import TwistParsing

main = do args <- getArgs
          case args of
               arg:[] -> do handle <- openFile (head args) ReadMode
                            contents <- hGetContents handle
                            let tree' = TwistParsing.parse contents
                            case tree' of
                                 Left  _    -> do putStrLn "Syntax error"
                                                  exitFailure
                                 Right tree -> do putStrLn "Standard unparse:"
                                                  putStrLn $ unparse tree
                                                  putStrLn "\nVerbose unparse:"
                                                  putStr $ unparseVerbose tree
               _      -> do progName <- getProgName
                            putStrLn $ "Usage: " ++ progName ++ " filename.q"
                            exitFailure
