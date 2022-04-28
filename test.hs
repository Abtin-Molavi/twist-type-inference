module Main where

import System.Environment
import System.Exit
import System.IO
import TwistParsing
import TwistTyping

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
                                                  putStrLn $ unparseVerbose tree
                                                  putStrLn "\nConstraint solution:"
                                                  print (solveConstraints (genConstraintsProg tree))
               _      -> do progName <- getProgName
                            putStrLn $ "Usage: " ++ progName ++ " filename.q"
                            exitFailure
