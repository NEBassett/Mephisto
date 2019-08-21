module Lib
    ( someFunc
    ) where

import MephistoTypes
import MephistoEval
import MephistoTypeCheck

someFunc :: IO ()
someFunc = putStrLn "someFunc"
