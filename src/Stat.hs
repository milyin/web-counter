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
import Data.Lens
import Data.Lens.Template
import Data.Lens.IxSet

newtype Url = Url B.ByteString  deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)
newtype DayHour = DayHour Int8 deriving (Eq, Ord, Read, Show, Data, Typeable, SafeCopy)

newtype Region = Region String
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data Visit = Visit {
    visitTime :: UTCTime,
    visitIp   :: Either IPv4 IPv6,
    visitUrl  :: Url
    }

data Stat = Stat {
    _statUrl    :: Url,
    _statDate   :: Day,
    _statHour   :: DayHour,
    _statRegion :: Region,
    _statCount  :: Int32
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''Stat)

instance Indexable Stat where
    empty = ixSet [ 
        ixFun' statUrl,
        ixFun' statDate,
        ixFun' statHour,
        ixFun $ \s -> [ (statDate ^$ s, statHour ^$ s) ],
        ixFun' statRegion
        ]
        where ixFun' lens = ixFun $ \a -> [ lens ^$ a ] 

data Stats = Stats {
    _stats :: IxSet Stat,
    _statsCount :: Int
    } deriving (Data, Typeable)

$(makeLens ''Stats)

initialStats = Stats { _stats = empty, _statsCount = 0 }

incStatsCount :: Update Stats Int
incStatsCount = do
    s <- get
    let new_s = (statsCount ^%= succ) s 
    put $ new_s
    return $ statsCount ^$ new_s

peekStatsCount :: Query Stats Int
peekStatsCount = getL statsCount <$> ask

-- incStats :: Visit -> Update Stats Int
-- incStats visit = do
--    s@Stats{..} <- get

$(makeAcidic ''Stats ['incStatsCount, 'peekStatsCount])
$(deriveSafeCopy 0 'base ''Region)
$(deriveSafeCopy 0 'base ''Stat)
$(deriveSafeCopy 0 'base ''Stats)

