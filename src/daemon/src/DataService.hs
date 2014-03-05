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
import qualified Data.IxSet as IxSet

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

queryDataService :: LocalNode -> ProcessId -> DataRequest -> IO DataResponse
queryDataService node srvpid req = do
    result <- newIORef $ RespOk
    runProcess node $ do
        self <- getSelfPid
        send srvpid (self, req)
        resp <- expect
        liftIO $ writeIORef result resp
    readIORef result

serverProc :: AcidState Stats -> Process ProcessId
serverProc acidStats = spawnLocal $ forever $ do 
    req <- expect :: Process (ProcessId, DataRequest)
    serve req
    where
    serve :: (ProcessId, DataRequest) -> Process ()
    serve (pid, PutVisit v) = send pid RespOk
    serve (pid, GetDbStat) = do
        stats <- liftIO $ query' acidStats PeekStats
        send pid $ RespDbStat $ DbStat (IxSet.size $ stats^.statsSet) (length $ stats^.visitsLog)
    serve (pid, _) = send pid RespOk
