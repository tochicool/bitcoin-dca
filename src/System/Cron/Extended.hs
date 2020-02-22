module System.Cron.Extended
    ( module System.Cron
    , previousMatch
    ) where

import           Data.Time.Clock   (UTCTime, addUTCTime)
import           System.Cron
import           System.Cron.Types (CronSchedule)

-- | Will return the previous time from the given starting point where
-- this schedule would have matched. Returns Nothing if the schedule will
-- never match. Note that this function is inclusive of the given
-- time.
previousMatch :: CronSchedule -> UTCTime -> Maybe UTCTime
previousMatch cs now
    -- This uses 'one-sided' binary search, making O(lg n) calls to nextMatch
 = do
    next <- nextMatch cs now
    let beforeNext time = do
            current <- nextMatch cs time
            if current == next
                then return time
                else beforeNext current
    let searchPrevious lookBehind = do
            current <- nextMatch cs (addUTCTime (-lookBehind) now)
            if current < next
                then beforeNext current
                else searchPrevious $ 2 * lookBehind
    searchPrevious 60
