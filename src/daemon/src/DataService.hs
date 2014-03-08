{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, GeneralizedNewtypeDeriving, 
    MultiParamTypeClasses, TemplateHaskell, TypeFamilies, RecordWildCards,
    DeriveGeneric #-}

module DataService where

import Control.Distributed.Process
import Control.Distributed.Process.Node(LocalNode)
import Control.Distributed.Process.Node(runProcess)
import Control.Lens
import Control.Monad
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import Data.Binary
import Data.IORef(newIORef)
import Data.IORef(readIORef)
import Data.IORef(writeIORef)
import Data.Int
import Data.Typeable
import GHC.Generics (Generic)
import Stat
import Geo
import qualified Data.IxSet as IxSet
import Control.Lens.TH(makeLenses)

data DbStat = DbStat { _statsSetSize :: Int, _visitsLogSize :: Int }
    deriving (Show, Eq, Typeable, Generic)

makeLenses ''DbStat

instance Binary DbStat

data DataRequest 
    = PutVisit Visit 
    | GetVisitsByDay Int 
    | GetVisitsByHour Int8
    | GetVisitsLog
    | ClearVisitsLog
    | GetDbStat
    deriving (Show, Eq, Typeable, Generic)

instance Binary Visit

instance Binary DataRequest

data DataResponse = RespOk 
                  | RespVisits Int 
                  | RespDbStat DbStat
                  | RespVisitsLog [Visit]
    deriving (Show, Eq, Typeable, Generic)

instance Binary DataResponse

queryData :: LocalNode -> ProcessId -> DataRequest -> IO DataResponse
queryData node srvpid req = do
    result <- newIORef $ RespOk
    runProcess node $ do
        self <- getSelfPid
        send srvpid (self, req)
        resp <- expect
        liftIO $ writeIORef result resp
    readIORef result

putData :: LocalNode -> ProcessId -> DataRequest -> IO ()
putData node srvpid req = do
    runProcess node $ do
        self <- getSelfPid
        send srvpid (self, req)

withAcidDbs :: (AcidState Stats -> AcidState GeoDb -> Process ()) -> Process ()
withAcidDbs f = 
    withAcid initialStats $ \stats ->
    withAcid initialGeoDb $ \geodb -> f stats geodb
    where
    withAcid init f = bracket (liftIO $ openLocalState init) (\db -> liftIO $ createCheckpointAndClose db) f

dbServerProc :: Process ProcessId
dbServerProc = spawnLocal $ withAcidDbs _dbServerProc

_dbServerProc stats geodb = forever $ do
    req <- expect :: Process (ProcessId, DataRequest)
    serve req
    where
        serve :: (ProcessId, DataRequest) -> Process ()
        serve (_, PutVisit v) = do
            liftIO $ update' stats $ RecordVisit v
        serve (pid, GetDbStat) = do
            stats <- liftIO $ query' stats PeekStats
            send pid $ RespDbStat $ DbStat (IxSet.size $ stats^.statsSet) (length $ stats^.visitsLog)
        serve (pid, GetVisitsLog) = do
            stats <- liftIO $ query' stats PeekStats
            send pid $ RespVisitsLog $ stats^.visitsLog 
        serve (pid, ClearVisitsLog) = do
            liftIO $ update' stats ClearVisits
        serve (pid, _) = send pid RespOk


