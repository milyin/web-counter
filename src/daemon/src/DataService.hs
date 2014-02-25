{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards,
    DeriveGeneric #-}

module DataService where

import Stat
import Control.Distributed.Process
import Data.Int
import Data.Binary
import GHC.Generics (Generic)
import Data.Typeable
import Control.Monad
import Stat
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import qualified Data.IxSet as IxSet
import Control.Lens

data DbStat = DbStat { statSetSize :: Int, visitsLogSize :: Int }
    deriving (Show, Eq, Typeable, Generic)

instance Binary DbStat

data DataRequest 
    = PutVisit Visit 
    | GetVisitsByDay Int 
    | GetVisitsByHour Int8
    | GetDbStat
    deriving (Show, Eq, Typeable, Generic)

instance Binary Visit

instance Binary DataRequest

data DataResponse = RespOk 
                  | RespVisits Int 
                  | RespDbStat DbStat
    deriving (Show, Eq, Typeable, Generic)

instance Binary DataResponse

serverProc :: AcidState Stats -> Process ProcessId
serverProc acidStats= spawnLocal $ forever $ do 
    req <- expect :: Process (ProcessId, DataRequest)
    serve req
    where
    serve :: (ProcessId, DataRequest) -> Process ()
    serve (pid, PutVisit v) = send pid RespOk
    serve (pid, GetDbStat) = do
        stats <- liftIO $ query' acidStats PeekStats
        send pid $ RespDbStat $ DbStat (IxSet.size $ stats^.statsSet) (length $ stats^.visitsLog)
    serve (pid, _) = send pid RespOk
