{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module DStatus (
    new,
    measure,
    logAccess,
    DStatus
) where

import Control.Monad.Trans
import Happstack.Server hiding ( logAccess )
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.Time.Format
import qualified Data.Map.Strict as Map
import Text.Blaze ((!))
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as C
import Data.Foldable
import Control.Applicative
import Network.URI
import Data.List.Split
import Data.Time
import Text.Printf

data Stat = Stat {
    loggedVisits :: Int,
    measuredVisits :: Int,
    maxTime :: NominalDiffTime,
    avgTime :: NominalDiffTime,
    totalTime :: NominalDiffTime
    }

data DStatus = DStatus { 
    stat :: Map.Map B.ByteString Stat
    }

instance H.ToMarkup DStatus where
    toMarkup = htmlDStatus
    preEscapedToMarkup = htmlDStatus

logVisit :: B.ByteString -> DStatus -> DStatus
logVisit page dstatus = dstatus { stat = Map.alter inc page $ stat dstatus }
    where 
        inc Nothing = Just $ Stat 1 0 0 0 0
        inc (Just stat) = Just $ stat { loggedVisits = 1 + loggedVisits stat }

measureVisit :: B.ByteString -> NominalDiffTime -> DStatus -> DStatus
measureVisit page time dstatus = dstatus { stat = Map.alter update page $ stat dstatus }
    where 
        update Nothing = Just $ Stat 0 1 time time time
        update (Just stat) = Just $ stat { 
            measuredVisits = newMeasuredVisits,
            maxTime = max (maxTime stat) time,
            totalTime = newTotalTime,
            avgTime = newTotalTime / (fromIntegral newMeasuredVisits)
            }
            where
            newMeasuredVisits = 1 + measuredVisits stat
            newTotalTime = time + totalTime stat

topPath url = case parseURIReference url of
    Nothing  -> ""
    Just uri -> head $ tail $ (splitOn "/" $ uriPath uri) ++ (repeat "")

measure :: (MonadIO m) => TVar DStatus -> Request -> UnWebT m a -> UnWebT m a
measure dvar rq m = do
    let page = C.pack $ topPath $ rqUri rq
    dstatus <- liftIO $ readTVarIO dvar
    start <- liftIO getCurrentTime
    job <- m
    stop <- job `seq` (liftIO getCurrentTime)
    let time = diffUTCTime stop start
    liftIO $ atomically $ modifyTVar dvar $ measureVisit page time     
    return job

new :: IO (TVar DStatus)
new = newTVarIO (DStatus Map.empty)

logAccess :: forall t . FormatTime t => TVar DStatus -> (LogAccess t) -> LogAccess t
logAccess dstatus logFunc host user time requestLine responseCode size referer userAgent = do
    let url = head $ tail $ (words requestLine) ++ (repeat "") -- /path from "GET /path 1.1"
    atomically $ modifyTVar dstatus $ logVisit $ C.pack $ topPath url
    logFunc host user time requestLine responseCode size referer userAgent

htmlDStatus :: DStatus -> H.Html
htmlDStatus dstatus = H.table ! A.border "1" $ do
    H.tr $ do
        H.td ! A.rowspan "2" $ H.toHtml $ ("page"::String)
        H.td ! A.colspan "2" $ H.toHtml $ ("visits"::String)
        H.td ! A.rowspan "2" $ H.toHtml $ ("avg time"::String)
        H.td ! A.rowspan "2" $ H.toHtml $ ("max time"::String)
        H.td ! A.rowspan "2" $ H.toHtml $ ("total time"::String)
    H.tr $ do
        H.td $ H.toHtml $ ("logged"::String)
        H.td $ H.toHtml $ ("processed"::String)
    forM_ (Map.toList $ stat dstatus) $ \(url,stat) -> do
        H.tr $ do
            H.td $ H.toHtml $ C.unpack url
            H.td $ H.toHtml $ loggedVisits stat
            H.td $ H.toHtml $ measuredVisits stat
            H.td $ H.toHtml $ show $ avgTime stat
            H.td $ H.toHtml $ show $ maxTime stat
            H.td $ H.toHtml $ show $ totalTime stat
