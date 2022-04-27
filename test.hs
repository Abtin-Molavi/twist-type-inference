import System.IO
import TwistParsing

main = do handle <- openFile "test.q" ReadMode
          contents <- hGetContents handle
          let tree = TwistParsing.parse contents
          print $ tree
          putStr "\n\n"
          putStr (unparse tree)
