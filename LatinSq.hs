module LatinSq
(
    generateReducedSquares,
    generateMolsFromReduced,
    latinsq
)
where

import System.IO ( hClose, hPutStrLn, openFile, hPrint, IOMode(WriteMode) );
import Utils ( (\\), trd, writeOrthogonalSquares, writeReducedSquares );

-- import System.Environment ( getArgs );
-- ghc --make -main-is LatinSq LatinSq.hs
-- main = do
--     (nstr : fileNameR : fileNameO : _) <- getArgs
--     let n = read nstr ::Int
--     latinsq n fileNameR fileNameO

latinsq :: Int -> FilePath -> FilePath -> IO ()
latinsq n fileNameR fileNameO = do
    -- reduced
    let reducedSquares = generateReducedSquares n
    handleR <- openFile fileNameR WriteMode
    hPrint handleR (length reducedSquares)
    hPutStrLn handleR ""
    writeReducedSquares handleR reducedSquares n
    hClose handleR

    -- mols
    let orthogonalSeq = generateMolsFromReduced reducedSquares n
    handleO <- openFile fileNameO WriteMode
    hPutStrLn handleO ("longest mols: "++ show (maximum $ map length orthogonalSeq)) -- longest mols
    hPutStrLn handleO ""
    writeOrthogonalSquares handleO orthogonalSeq n
    hClose handleO

generateReducedSquares :: Int -> [[(Int, Int, Int)]]
generateReducedSquares n = generateReducedSquares' (getFirstRowAndCol n) (2, 2) n

generateReducedSquares' :: [(Int, Int, Int)] -> (Int, Int) -> Int -> [[(Int, Int, Int)]]
generateReducedSquares' square (row, _) n
    | row == n + 1 = [square]
generateReducedSquares' current (row, col) n = 
    concatMap 
    (\el -> generateReducedSquares' ((row, col, el) : current) (getNextPos row col n 2) n) 
    (getPossibleElements current row col n)

getNextPos :: Int -> Int -> Int -> Int -> (Int, Int)
getNextPos row col n bg
    | col == n = (row + 1, bg)
    | otherwise = (row, col + 1)

getPossibleElements :: [(Int, Int, Int)] -> Int -> Int -> Int -> [Int]
getPossibleElements current row col n = [1..n] \\ map trd (filter (\(i,j,_) -> (i == row) || (j == col)) current)

getFirstRow :: Int -> [(Int, Int, Int)]
getFirstRow n = zip3 (replicate n 1) [1..] [1..]

getFirstRowAndCol :: Int -> [(Int, Int, Int)]
getFirstRowAndCol n = getFirstRow n ++ zip3 [2..] (replicate (n-1) 1) [2..]

getFirstRowPairs :: Int -> [(Int, Int)]
getFirstRowPairs n = zip [1..n] [1..n]

generateMolsFromReduced :: [[(Int, Int, Int)]] -> Int -> [[[(Int, Int, Int)]]]
generateMolsFromReduced reduced n = map (\rsq -> generateMols [] n [rsq]) reduced 

generateMols :: [[(Int, Int, Int)]] -> Int -> [[(Int, Int, Int)]] -> [[(Int, Int, Int)]]
generateMols mols _ [] = mols
generateMols _ 1 next = next
generateMols mols n next = 
    generateMols updatedMols n (generateNextOrthogonalSquare n updatedMols (map (\_ -> getFirstRowPairs n) updatedMols) (getFirstRow n) (2, 1))
    where updatedMols = head next : mols

generateNextOrthogonalSquare :: Int -> [[(Int, Int, Int)]] -> [[(Int, Int)]] -> [(Int, Int, Int)] -> (Int, Int) -> [[(Int, Int, Int)]]
generateNextOrthogonalSquare n _ _ square (row, _)
    | row == n + 1 = [square]
generateNextOrthogonalSquare n mols pairs current (row, col) = 
    take 1 $ 
    concatMap 
    (\el -> generateNextOrthogonalSquare n mols (zipWith ((:) . (, el)) molsEl pairs) ((row, col, el) : current) (getNextPos row col n 1)) 
    (getPossibleElementsMols molsEl current pairs row col n)
    where molsEl = getMolsElemAt mols row col

getMolsElemAt :: [[(Int, Int, Int)]] -> Int -> Int -> [Int]
getMolsElemAt mols row col = map ((trd . head) . filter (\ (i, j, _) -> i == row && j == col)) mols

getPossibleElementsMols :: [Int] -> [(Int, Int, Int)] -> [[(Int, Int)]] -> Int -> Int -> Int -> [Int]
getPossibleElementsMols molsEl current pairs row col n = foldr (\(p, el) acc -> map snd (map (el,) acc \\ p)) possible (zip pairs molsEl)
    where possible = getPossibleElements current row col n

