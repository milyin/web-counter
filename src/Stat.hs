{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards #-}

module Stat where

import Data.IP
import Data.Time
import Data.Time.Calendar
import Data.Int
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
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
import Geo

newtype Url = Url B.ByteString  deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
newtype DayHour = DayHour Int8 deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

newtype Region = Region String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Visit = Visit {
    vzTime     :: UTCTime,
    vzClientIp :: String,
    vzReferer  :: B.ByteString
    } deriving (Show)

data StatIndex = StatIndex {
    _statUrl    :: Url,
    _statDay    :: Day,
    _statHour   :: DayHour,
    _statRegion :: Region
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''StatIndex)

makeIndex :: Visit -> StatIndex
makeIndex visit = StatIndex {
    _statUrl    = Url $ vzReferer $ visit,
    _statDay    = localDay visitLocalTime,
    _statHour   = localDayHour visitLocalTime,
    _statRegion = ip2region $ vzClientIp visit
    } where
        visitLocalTime = utcToLocalTime utc $ vzTime visit
        localDayHour localTime = DayHour $ fromIntegral $ todHour $ localTimeOfDay localTime

ip2region :: String -> Region
ip2region s = Region s

data Stat = Stat {
    _statIndex  :: StatIndex,
    _statCount  :: Int  -- visitors count for this 'statIndex'
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
    _statsSet :: IxSet Stat
    } deriving (Data, Typeable)

$(makeLens ''Stats)

stat :: (Typeable key) => key -> Lens (IxSet Stat) (Maybe Stat)
stat key = ixLens key

initialStats = Stats { _statsSet = empty }

incStats :: StatIndex -> Update Stats Int
incStats index = do
    s <- get
    let rec = maybe (Stat index 1) (statCount ^%= succ) (stat index ^$ statsSet ^$ s)
    let new_s = (statsSet ^%= updateIx index rec) s
    put $ new_s
    return $ statCount ^$ rec

peekStats :: Query Stats (IxSet Stat)
peekStats = getL statsSet <$> ask

$(makeAcidic ''Stats ['incStats, 'peekStats])
$(deriveSafeCopy 0 'base ''Region)
$(deriveSafeCopy 0 'base ''StatIndex)
$(deriveSafeCopy 0 'base ''Stat)
$(deriveSafeCopy 0 'base ''Stats)

