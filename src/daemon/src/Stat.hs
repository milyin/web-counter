{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards,
    DeriveGeneric #-}

module Stat where

import Data.IP
import Data.Time
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.IxSet hiding (Indexable)
import qualified Data.IxSet as IxSet (Indexable)
import Control.Monad
import Control.Monad.State ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative ( (<$>) )
import Geo
import Control.Lens hiding ( (|>), (<|) )
import Data.Sequence (Seq, (|>), (<|) )
import qualified Data.Sequence as Seq
import GHC.Generics (Generic)

newtype Url = Url B.ByteString  deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
newtype DayHour = DayHour Int8 deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

data Visit = Visit {
    _visitTime      :: Int64,
    _visitIp        :: B.ByteString,
    _visitReferer   :: B.ByteString,
    _visitUserAgent :: B.ByteString,
    _visitR         :: B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

makeLenses ''Visit

mkVisit time ip referer agent r = Visit {
    _visitTime = time,
    _visitIp   = ip,
    _visitReferer = referer,
    _visitUserAgent = agent,
    _visitR = r
    }

data StatIndex = StatIndex {
    _statUrl    :: Url,
    _statDay    :: Day,
    _statHour   :: DayHour
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeLenses ''StatIndex

visitToStatIndex :: Visit -> StatIndex
visitToStatIndex visit = StatIndex {
    _statUrl    = Url $ visit ^. visitReferer,
    _statDay    = localDay visitLocalTime,
    _statHour   = localDayHour visitLocalTime
    } where
        visitLocalTime = utcToLocalTime utc $ posixSecondsToUTCTime $ realToFrac $ visit ^. visitTime
        localDayHour localTime = DayHour $ fromIntegral $ todHour $ localTimeOfDay localTime

data Stat = Stat {
    _statIndex  :: StatIndex,
    _statCount  :: Int  -- visitors count for this 'statIndex'
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeLenses ''Stat

instance IxSet.Indexable Stat where
    empty = ixSet [ 
        ixFun $ \s -> [ s^.statIndex ],
        ixFun $ \s -> [ s^.statIndex.statUrl ],
        ixFun $ \s -> [ s^.statIndex.statDay ],
        ixFun $ \s -> [ s^.statIndex.statHour ],
        ixFun $ \s -> [ (s^.statIndex.statDay, s^.statIndex.statHour) ]
        ]

data Stats = Stats {
    _statsSet :: IxSet Stat,
    _visitsLog :: [ Visit ]
    } deriving (Data, Typeable)

makeLenses ''Stats

initialStats = Stats { _statsSet = empty, _visitsLog = [] }

recordVisit :: Visit -> Update Stats ()
recordVisit visit = do
    let index = visitToStatIndex visit
    s <- get
    let rec = maybe (Stat index 1) (statCount %~ succ) (getOne $ getEQ index $ s^.statsSet)
    let s1 = s & statsSet %~ updateIx index rec
    let s2 = s & visitsLog %~ \log -> visit:log
    put $ s2
    return ()

clearVisits :: Update Stats ()
clearVisits = do
    put $ initialStats
    return ()

peekStats :: Query Stats Stats
peekStats =  ask

$(makeAcidic ''Stats ['recordVisit, 'peekStats, 'clearVisits])
$(deriveSafeCopy 0 'base ''StatIndex)
$(deriveSafeCopy 0 'base ''Visit)
$(deriveSafeCopy 0 'base ''Stat)
$(deriveSafeCopy 0 'base ''Stats)

