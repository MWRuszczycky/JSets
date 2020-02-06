module Model.Core.Core
    ( txt
    , readMaybeTxt
    , chunksOf
    , collate
    , shuffleIn
    , shuffleInAt
    , shuffleTogether
    , takeEveryAt
    ) where

import qualified Data.Text as Tx
import           Data.Text       ( Text      )
import           Text.Read       ( readMaybe )

txt :: Show a => a -> Text
txt = Tx.pack . show

readMaybeTxt :: Read a => Text -> Maybe a
readMaybeTxt = readMaybe . Tx.unpack

chunksOf :: Int -> [a] -> [[a]]
-- ^Break a list up into sublists of n elements each. Any extra
-- elements are appended so this works with infinite lists.
chunksOf _ [] = []
chunksOf n xs = ys : chunksOf n zs
    where (ys, zs) = splitAt n xs

collate :: Int -> [[a]] -> [a]
-- ^Combine lists with n element of each list after the other. Any
-- overhang is deleted. For example,
-- collate 2 [ "aaaa", "bbbb", "cccc" ]
-- will return "aabbccaabbcc"
collate n = snd . foldr go (0,[])
    where go x (k,xs) = (k+n, shuffleInAt k n xs x)

shuffleIn :: [a] -> [a] -> [a]
-- ^Combine two lists with each element one after the other. Any
-- overhang is deleted. This will work with infinite lists.
shuffleIn []     _      = []
shuffleIn _      []     = []
shuffleIn (x:xs) (y:ys) = x : y : shuffleIn xs ys

shuffleTogether :: [[a]] -> [[a]] -> [[a]]
shuffleTogether [] _          = []
shuffleTogether _  []         = []
shuffleTogether (x:xs) (y:ys) = (x ++ y) : shuffleTogether xs ys

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
