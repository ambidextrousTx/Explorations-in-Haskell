-- Automatic geometric series generator 
-- Very limited for now, takes a fixed format input
-- Produces an infinite list
-- Example: take 3 (gs [3, 15..]) => [3, 15, 75]
-- Improvements aimed for: 
-- *accept upper limit
-- *handle data types properly, don't use truncate -> DONE (Sep 28, 2010)

gs :: (Integral a) => [a] -> [a]
gs (x:y:_) = [ x * (y `div` x) ^ n | n <- [0,1..] ] 

collatz2 :: (Integral i) => i -> i
collatz2 i
    | odd i = 3*i + 1
    | otherwise = i `div` 2
    
composed :: (Num a, Integral i) => a -> i -> i
composed n i
        | n == 1 = collatz2 i
        | otherwise = collatz2 (composed (n-1) i)
    
-- Generates the series [collatz n, (collatz.collatz) n, ...] up to 1
q4 :: (Integral i) => i -> [i]
q4 i
    | i > 0 = takeWhile (>1) [composed n i | n <- [1,2..]]
    | otherwise = [1]

-- Generates all possible splits of a list
q2 :: [a] -> [([a], [a])]
q2 xs =
    let n = length xs
    in [splitAt m xs | m <- [0..n]]

-- Converts decimal to binary
convert2Bin :: Int -> [Int]
convert2Bin n
    | n == 1 = [1]
    | otherwise = (n `mod` 2):convert2Bin (n `div` 2)


