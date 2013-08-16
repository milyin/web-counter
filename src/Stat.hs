{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Stat where

import Data.IP
import Data.Time
import Data.Time.Calendar
import Data.Int
import qualified Data.ByteString as B
import Data.Acid
import Data.SafeCopy
import Data.Data
import Data.IxSet
import Control.Monad
import Control.Monad.State ( get, put )
import Control.Monad.Reader ( ask )
import Control.Applicative ( (<$>) )
import Data.Lens.Common
import Data.Lens.Template (makeLens)
import Data.Lens.IxSet (ixLens)

newtype Url = Url B.ByteString  deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
newtype DayHour = DayHour Int8 deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

localDayHour :: LocalTime -> DayHour
localDayHour localTime = DayHour $ fromIntegral $ todHour $ localTimeOfDay localTime

newtype Region = Region String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Visit = Visit {
    visitTime :: UTCTime,
    visitIp   :: Either IPv4 IPv6,
    visitUrl  :: Url
    }

data StatIndex = StatIndex {
    _statUrl    :: Url,
    _statDay    :: Day,
    _statHour   :: DayHour,
    _statRegion :: Region
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''StatIndex)

visit2index :: TimeZone -> Visit -> StatIndex
visit2index tz visit = StatIndex {
    _statUrl    = visitUrl visit,
    _statDay    = localDay visitLocalTime,
    _statHour   = localDayHour visitLocalTime,
    _statRegion = ip2region $ visitIp visit
    } where
        visitLocalTime = utcToLocalTime tz $ visitTime visit

ip2region :: (Either IPv4 IPv6) -> Region
ip2region (Left _) = Region "IPv4"
ip2region (Right _) = Region "IPv6"

data Stat = Stat {
    _statIndex  :: StatIndex,
    _statCount  :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''Stat)

instance Indexable Stat where
    empty = ixSet [ 
        ixFun $ \s -> [               statIndex ^$ s ],
        ixFun $ \s -> [ statUrl    ^$ statIndex ^$ s ],
        ixFun $ \s -> [ statDay    ^$ statIndex ^$ s ],
        ixFun $ \s -> [ statHour   ^$ statIndex ^$ s ],
        ixFun $ \s -> [ statRegion ^$ statIndex ^$ s ],
        ixFun $ \s -> [ (statDay   ^$ statIndex ^$ s, statHour ^$ statIndex ^$ s) ]
        ]

data Stats = Stats {
    _statsSet :: IxSet Stat,
    _statsCount :: Int
    } deriving (Data, Typeable)

$(makeLens ''Stats)

stat :: (Typeable key) => key -> Lens (IxSet Stat) (Maybe Stat)
stat key = ixLens key

initialStats = Stats { _statsSet = empty, _statsCount = 0 }

incStatsCount :: Update Stats Int
incStatsCount = do
    s <- get
    let new_s = (statsCount ^%= succ) s 
    put $ new_s
    return $ statsCount ^$ new_s

peekStatsCount :: Query Stats Int
peekStatsCount = getL statsCount <$> ask

incStats :: StatIndex -> Update Stats Int
incStats index = do
    s <- get
    let rec = maybe (Stat index 1) (statCount ^%= succ) (stat index ^$ statsSet ^$ s)
    let new_s = (statsSet ^%= updateIx index rec) s
    put $ new_s
    return $ statCount ^$ rec

$(makeAcidic ''Stats ['incStatsCount, 'peekStatsCount, 'incStats])
$(deriveSafeCopy 0 'base ''Region)
$(deriveSafeCopy 0 'base ''StatIndex)
$(deriveSafeCopy 0 'base ''Stat)
$(deriveSafeCopy 0 'base ''Stats)

