module Main where
import Data.Maybe
import System.Environment
import Lib

fDict = [("d1",d1),("d1b",d1b)]

main :: IO ()
main = do
    (day:args) <- getArgs
    let (Just func) = lookup day fDict
    interact func