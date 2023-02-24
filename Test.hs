module Test where

import LatinSq;
import Utils;

getColsOrRows sq n p = map (\idx -> map trd (filter (\x -> p x == idx) sortedSq)) [1..n]
    where sortedSq = quickSort sq

getCols sq n = getColsOrRows sq n snd'
getRows sq n = getColsOrRows sq n fst'

exactlyOne sq n = all (\xs -> null ([1..n] \\ xs))
exactlyOneInCols sq n = exactlyOne sq n (getCols sq n)
exactlyOneInRows sq n = exactlyOne sq n (getRows sq n)

-- map (flip isLatinSquare 4) (getReducedSquares 4)
isLatinSquare sq n = exactlyOneInCols sq n && exactlyOneInRows sq n
allLatinSquares sqs n = all (flip isLatinSquare n) sqs

getOrthPairs sq1 sq2 = zipWith (\(_, _, x) (_, _, y) -> (x, y)) (quickSort sq1) (quickSort sq2)

containsDuplicate :: Eq a => [a] -> Bool
containsDuplicate [] = False
containsDuplicate (x : xs) = (x `elem` xs) || containsDuplicate xs

areTwoLsOrthogoanl sq1 sq2 = not $ containsDuplicate pairs
    where pairs = getOrthPairs sq1 sq2

-- allMutuallyOrthogonal :: (Eq b2, Ord a, Ord b1) => [[(a, b1, b2)]] -> Bool
allMutuallyOrthogonal mols = and [areTwoLsOrthogoanl sq1 sq2 | sq1 <- mols, sq2 <- mols, sq1/=sq2]

testCase nr expected actual =
    if expected == actual then
        putStrLn (show nr ++ ": pass")
    else 
        putStrLn (show nr ++ ": fail")

tests = do
    let reduced1 = generateReducedSquares 1
    let reduced2 = generateReducedSquares 2
    let reduced3 = generateReducedSquares 3
    let reduced4 = generateReducedSquares 4
    let reduced5 = generateReducedSquares 5
    let reduced6 = generateReducedSquares 6
    let reduced10 = take 10 (generateReducedSquares 10)

    putStrLn "testing reduced..."
    putStrLn "testing sizes..."
    testCase 1 (length reduced1) 1
    testCase 2 (length reduced2) 1
    testCase 3 (length reduced3) 1
    testCase 4 (length reduced4) 4
    testCase 5 (length reduced5) 56
    -- testCase 6 (length reduced6) 9408

    putStrLn "testing if latin squares..."
    testCase 1 (allLatinSquares reduced1 1) True
    testCase 2 (allLatinSquares reduced2 2) True
    testCase 3 (allLatinSquares reduced3 3) True
    testCase 4 (allLatinSquares reduced4 4) True
    testCase 5 (allLatinSquares reduced5 5) True
    testCase 10 (allLatinSquares reduced10 10) True

    putStrLn ""
    putStrLn "testing mols..."
    let orthSeq1 = generateMolsFromReduced reduced1 1
    let orthSeq2 = generateMolsFromReduced reduced2 2
    let orthSeq3 = generateMolsFromReduced reduced3 3
    let orthSeq4 = generateMolsFromReduced reduced4 4
    let orthSeq5 = generateMolsFromReduced reduced5 5
    let orthSeq6 = generateMolsFromReduced (take 3 reduced6) 6

    putStrLn "testing if latin squares..."
    testCase 1 (all (flip allLatinSquares 1) orthSeq1) True
    testCase 2 (all (flip allLatinSquares 2) orthSeq2) True
    testCase 3 (all (flip allLatinSquares 3) orthSeq3) True
    testCase 4 (all (flip allLatinSquares 4) orthSeq4) True
    testCase 5 (all (flip allLatinSquares 5) orthSeq5) True
    testCase 6 (all (flip allLatinSquares 6) (take 1 orthSeq6)) True

    putStrLn "testing if mutually orthogonal..."
    testCase 1 (all allMutuallyOrthogonal orthSeq1) True
    testCase 2 (all allMutuallyOrthogonal orthSeq2) True
    testCase 3 (all allMutuallyOrthogonal orthSeq3) True
    testCase 4 (all allMutuallyOrthogonal orthSeq4) True
    testCase 5 (all allMutuallyOrthogonal orthSeq5) True
    testCase 6 (all allMutuallyOrthogonal (take 1 orthSeq6)) True

    return ()