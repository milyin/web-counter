{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards, StandaloneDeriving #-}

module Geo where

import Data.IP()
import Data.IxSet hiding (Indexable)
import qualified Data.IxSet as IxSet (Indexable)
import Data.Data
import Data.IxSet()
import Data.SafeCopy
import Data.Acid
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

data Location = Location {
    _lcCountry :: Int,
    _lcRegion  :: Int,
    _lcCity    :: Int
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeLenses ''Location

data IpRange = IpRange4 [Int] [Int] | IpRange6 [Int] [Int]
    deriving (Eq, Ord, Read, Show, Data, Typeable)

data IpRangeLocation = IpRangeLocation {
    _irlRange    :: IpRange,
    _irlLocation :: Location
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeLenses ''IpRangeLocation

instance IxSet.Indexable IpRangeLocation where
    empty = ixSet [ 
        ]

data Reference = Reference {
    _refId :: Int,
    _refValue :: B.ByteString
    } deriving (Eq, Ord, Read, Show, Data, Typeable)

makeLenses ''Reference

instance IxSet.Indexable Reference where
    empty = ixSet [ 
        ]

data GeoDb = GeoDb {
    _dbCountries :: IxSet Reference,
    _dbRegions   :: IxSet Reference,
    _dbCities    :: IxSet Reference,
    _dbRanges    :: IxSet IpRangeLocation
    } deriving (Data, Typeable)

initialGeoDb = GeoDb empty empty empty empty

makeLenses ''GeoDb
makeAcidic ''GeoDb []
deriveSafeCopy 0 'base ''GeoDb
deriveSafeCopy 0 'base ''Reference
deriveSafeCopy 0 'base ''IpRangeLocation
deriveSafeCopy 0 'base ''IpRange
deriveSafeCopy 0 'base ''Location

