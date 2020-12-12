module Model.Core.Core
    ( -- Working with Text strings
      tshow
    , readMaybeTxt
    , readTxt
      -- Working with file paths
    , extension
    , readFormat
      -- List manipulation
    , chunksOf
    , takeEveryAt
    , collate
    , shuffleIn
    , shuffleInAt
    , zipLists
    , addUnique
    , splitMaybe
    , choice
    ) where

import qualified Data.Text           as Tx
import qualified Model.Core.Types    as T
import           Control.Applicative        ( (<|>), Alternative (..) )
import           Data.Char                  ( toLower                 )
import           Data.List                  ( foldl'                  )
import           Data.Text                  ( Text                    )
import           Text.Read                  ( readMaybe               )

-- =============================================================== --
-- Working with Text strings

tshow :: Show a => a -> Text
tshow = Tx.pack . show

readMaybeTxt :: Read a => Text -> Maybe a
readMaybeTxt = readMaybe . Tx.unpack

readTxt :: Read a => Text -> a
readTxt = read . Tx.unpack

-- =============================================================== --
-- Working with file paths

extension :: FilePath -> String
extension fp
    | null p    = []
    | otherwise = reverse e
    where (e,p) = break (=='.') . reverse $ fp

readFormat :: String -> Maybe T.Format
readFormat fmt = case map toLower fmt of
                      "csv"  -> Just T.CSV
                      "mkd"  -> Just T.MKD
                      "md"   -> Just T.MKD
                      "html" -> Just T.HTML
                      "txt"  -> Just T.TXT
                      _      -> Nothing

-- =============================================================== --
-- List manipulation

---------------------------------------------------------------------
-- Breaking lists up

chunksOf :: Int -> [a] -> [[a]]
-- ^Break a list up into sublists of n elements each. Any extra
-- elements are appended so this works with infinite lists.
chunksOf _ [] = []
chunksOf n xs
    | n < 1     = []
    | otherwise = ys : chunksOf n zs
    where (ys, zs) = splitAt n xs

takeEveryAt :: Int -> Int -> [a] -> [[a]]
-- ^Take a chunk of n elements after every m elements in the list.
-- The first chunk taken is from element 0. The last chunk may have
-- fewer than n elements if the end of the list is reached. If n < 1
-- return a list of empty lists one for every m in xs. If there is
-- any overhang, then an additional empty list will be appended.
takeEveryAt _ _ [] = []
takeEveryAt n m xs = (:) us . takeEveryAt n m . drop m $ vs
    where (us,vs) = splitAt n xs

---------------------------------------------------------------------
-- Combining lists

collate :: Int -> [[a]] -> [a]
-- ^Combine lists with n elements of each list after the other. Any
-- overhang is deleted. For example,
-- collate 2 [ "aaaa", "bbbb", "cccc" ]
-- will return "aabbccaabbcc"
collate n = snd . foldl' go (0,[])
    where go (k,xs) x = (k+n, shuffleInAt k n xs x)

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

zipLists :: [[a]] -> [[a]] -> [[a]]
-- ^Take two lists of lists and zip them together concatenating the
-- the paired lists. This is the list-analog of zip.
zipLists [] _          = []
zipLists _  []         = []
zipLists (x:xs) (y:ys) = (x <> y) : zipLists xs ys

addUnique :: Eq a => [a] -> [a] -> [a]
-- ^Union where the second list is known to contain no duplicates.
addUnique []     xs = xs
addUnique (y:ys) xs
    | elem y xs = addUnique ys xs
    | otherwise = addUnique ys (y:xs)

---------------------------------------------------------------------
-- Decomposing lists

splitMaybe :: [(a, Maybe b)] -> ([(a,b)], [a])
splitMaybe = foldr go ([],[])
    where go (k, Just v ) (js,ns) = ( (k,v):js, ns  )
          go (k, Nothing) (js,ns) = ( js,       k:ns)

choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty
