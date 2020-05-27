{-# LANGUAGE OverloadedStrings #-}

module Model.Core.Dates
    ( today
    , firstOfYear
    , firstOfNextYear
    , sameYear
    , diffYears
    , diffMonths
    , diffDays
      -- time
    , readClock
    , deltaClock
    , wait
    ) where

import qualified Data.Time          as Tm
import qualified Model.Core.Types   as T
import           Control.Concurrent       ( threadDelay )
import           Data.Time                ( Day         )

-- =============================================================== --
-- Working with dates

today :: IO Day
-- ^Determine the current.
today = Tm.utctDay <$> Tm.getCurrentTime

firstOfYear :: Day -> Day
firstOfYear d = Tm.fromGregorian (fromIntegral $ T.year d) 1 1

firstOfNextYear :: Day -> Day
firstOfNextYear d = Tm.fromGregorian ((+1) . fromIntegral . T.year $ d) 1 1

sameYear :: Day -> Day -> Bool
sameYear d1 d2 = T.year d1 == T.year d2

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

-- =============================================================== --
-- Working with time

readClock :: IO Integer
-- ^Picoseconds from midnight.
readClock = Tm.utctDayTime <$> Tm.getCurrentTime
            >>= pure . Tm.diffTimeToPicoseconds

deltaClock :: Integer -> IO Integer
-- ^Returns the difference between a start time and the call time in
-- units of picoseconds. Use with checkClock to get the start time.
deltaClock start = readClock >>= pure . subtract start

wait :: Integer -> IO ()
-- ^Delay current thread by n picoseconds.
wait n | n > 0     = threadDelay u
       | otherwise = pure ()
       where u = round $ fromIntegral n / 10^^6
