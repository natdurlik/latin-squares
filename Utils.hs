module Utils where

import System.IO;

(\\) :: (Foldable t, Eq a) => [a] -> t a -> [a]
(\\) xs ys = filter (not . (`elem` ys)) xs

fst' :: (a, b, c) -> a
fst' (x, _, _) = x
snd' :: (a, b, c) -> b
snd' (_, y, _) = y
trd :: (a, b, c) -> c
trd (_, _, z) = z

compareTriples :: (Num a1, Ord a2, Ord a3) => (a2, a3, c1) -> (a2, a3, c2) -> a1
compareTriples (a,b,_) (x,y,_)
    | a < x = 1
    | a > x = -1
    | b < y = 1
    | b > y = -1
    | otherwise = 0

quickSort :: (Ord a2, Ord a3) => [(a2, a3, c1)] -> [(a2, a3, c1)]
quickSort [] = []
quickSort (x:xs) = quickSort (filter ((<1) . compareTriples x) xs) ++ [x] ++ quickSort (filter ((==1) . compareTriples x) xs)

writeSquare :: (Show a1, Eq t) => Handle -> [(a2, t, a1)] -> t -> IO ()
writeSquare _ [] _ = return ()
writeSquare handle ((_, col, el):sq) n = do
    hPutStr handle (show el)
    if col == n then hPutStr handle "\n" else hPutStr handle " "
    writeSquare handle sq n

writeOrthogonalSquares :: (Show a1, Ord a2, Ord t) => Handle -> [[[(a2, t, a1)]]] -> t -> IO ()
writeOrthogonalSquares _ [] _ = return ()
writeOrthogonalSquares handle (mols : molss) n = do
    hPrint handle (length mols)
    writeReducedSquares handle (reverse mols) n
    hPutStrLn handle "----------------------------------------------"
    hPutStrLn handle ""
    writeOrthogonalSquares handle molss n

writeReducedSquares :: (Show a1, Ord a2, Ord t) => Handle -> [[(a2, t, a1)]] -> t -> IO ()
writeReducedSquares _ [] _ = return ()
writeReducedSquares handle (rsq : rsqs) n = do
    writeSquare handle (quickSort rsq) n
    hPutStrLn handle ""
    writeReducedSquares handle rsqs n