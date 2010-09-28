-- Automatic geometric series generator 
-- Very limited for now, takes a fixed format input
-- Produces an infinite list
-- Example: take 3 (gs [3, 15..]) => [3, 15, 75]
-- Improvements aimed for: 
-- *accept upper limit
-- *handle data types properly, don't use truncate

gs :: (Integral a) => [a] -> [b]
gs (x:y:_) = [ x * (y `div` x) ^ n | n <- [0,1..] ] 


