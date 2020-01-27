module Model.Core
    ( chunksOf
    , shuffleIn
    , shuffleInAt
    , takeEveryAt
    ) where

chunksOf :: Int -> [a] -> [[a]]
-- ^Break a list up into sublists of n elements each. Any extra
-- elements are appended so this works with infinite lists.
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
    where (ys, zs) = splitAt n xs

shuffleIn :: [a] -> [a] -> [a]
-- ^Combine two lists with each element one after the other. Any
-- overhang is deleted. This will work with infinite lists.
shuffleIn []     _      = []
shuffleIn _      []     = []
shuffleIn (x:xs) (y:ys) = x : y : shuffleIn xs ys

shuffleInAt :: Int -> Int -> [a] -> [a] -> [a]
-- ^Combine two lists with m elements from the second after every
-- n elements from the first. Overhang is deleted, and this works
-- with infinite lists. Negative values of n or m are treated as 0.
shuffleInAt n m xs ys
    | n < 1 && m < 1 = []
    | n < 1          = ys
    | m < 1          = xs
    | length xs1 < n = []
    | length ys1 < m = []
    | otherwise      = xs1 ++ ys1 ++ shuffleInAt n m xs2 ys2
    where (xs1,xs2) = splitAt n xs
          (ys1,ys2) = splitAt m ys

takeEveryAt :: Int -> Int -> [a] -> [[a]]
-- ^Take a chunk of n elements after every m elements in the list.
-- The first chunk taken is from element 0. The last chunk may have
-- fewer than n elements if the end of the list is reached.
takeEveryAt n m xs = go True xs
    where go _ [] = []
          go k xs | k         = let (us,vs) = splitAt n xs in us : go False vs
                  | otherwise = go True . drop m $ xs
