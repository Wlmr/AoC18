module Main where
import Data.Maybe
import System.Environment
import Lib

fDict = [("d1a",d1a),("d1b",d1b)]

main :: IO ()
main = do
    (day:args) <- getArgs
    let (Just func) = lookup day fDict
    interact func