{-# LANGUAGE OverloadedStrings #-}

module Model.Dates
    ( today
    , getYear
    , getMonth
    , firstOfYear
    , firstOfNextYear
    , sameYear
    , diffYears
    , diffMonths
    , diffDays
    ) where

import qualified Data.Time       as Tm
import           Data.Time              ( Day )

-- =============================================================== --
-- Working with dates

today :: IO Day
-- ^Determine the current.
today = Tm.utctDay <$> Tm.getCurrentTime

getYear :: Day -> Int
getYear d = let (y,_,_) = Tm.toGregorian d in fromIntegral y

getMonth :: Day -> Int
getMonth d = let (_,m,_) = Tm.toGregorian d in m

firstOfYear :: Day -> Day
firstOfYear d = Tm.fromGregorian (fromIntegral $ getYear d) 1 1

firstOfNextYear :: Day -> Day
firstOfNextYear d = Tm.fromGregorian ((+1) . fromIntegral . getYear $ d) 1 1

sameYear :: Day -> Day -> Bool
sameYear d1 d2 = getYear d1 == getYear d2

diffYears :: Day -> Day -> Int
diffYears d1 d0
    | y0 > y1   = 0
    | otherwise = fromIntegral $ y1 - y0
    where (y0,_,_) = Tm.toGregorian d0
          (y1,_,_) = Tm.toGregorian d1

diffMonths :: Day -> Day -> Int
diffMonths d1 d0
    | d1 < d0   = 0
    | otherwise = 12 * (fromIntegral $ y1 - y0) + (m1 - m0)
    where (y0,m0,_) = Tm.toGregorian d0
          (y1,m1,_) = Tm.toGregorian d1

diffDays :: Day -> Day -> Int
diffDays d1 d0
    | d1 < d0   = 0
    | otherwise = fromIntegral $ Tm.diffDays d1 d0
