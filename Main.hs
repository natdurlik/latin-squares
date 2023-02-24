module Main where

import LatinSq;
import System.Environment ( getArgs );

main = do
    (nstr : fileNameR : fileNameO : _) <- getArgs
    let n = read nstr ::Int
    latinsq n fileNameR fileNameO
    return ()