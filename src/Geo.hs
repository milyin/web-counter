{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards, StandaloneDeriving #-}

module Geo where

import Data.IP
import Data.IxSet
import Data.Data
import Data.IxSet
import Data.SafeCopy
import Data.Acid
import Data.Lens.Common
import Data.Lens.Template (makeLens)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Location = Location {
    _lcCountry :: Int,
    _lcRegion  :: Int,
    _lcCity    :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''Location)

data IpRange = IpRange4 [Int] [Int] | IpRange6 [Int] [Int]
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data IpRangeLocation = IpRangeLocation {
    _irlRange    :: IpRange,
    _irlLocation :: Location
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''IpRangeLocation)

instance Indexable IpRangeLocation where
    empty = ixSet [ 
        ]

data Reference = Reference {
    _refId :: Int,
    _refValue :: B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

$(makeLens ''Reference)

instance Indexable Reference where
    empty = ixSet [ 
        ]

data GeoDb = GeoDb {
    _dbCountries :: IxSet Reference,
    _dbRegions   :: IxSet Reference,
    _dbCities    :: IxSet Reference,
    _dbRanges    :: IxSet IpRangeLocation
    } deriving (Data, Typeable)

initialGeoDb = GeoDb empty empty empty empty

$(makeLens ''GeoDb)

$(makeAcidic ''GeoDb [])
$(deriveSafeCopy 0 'base ''GeoDb)
$(deriveSafeCopy 0 'base ''Reference)
$(deriveSafeCopy 0 'base ''IpRangeLocation)
$(deriveSafeCopy 0 'base ''IpRange)
$(deriveSafeCopy 0 'base ''Location)

