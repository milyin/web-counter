{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import qualified DStatus
import DStatus (measure, DStatus)
import qualified Stat
import Control.Exception.Base
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad
import Control.Monad.Trans
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import Data.List
import Stat
import Data.Time.LocalTime
import Data.Time.Clock
import Data.IxSet

data PointImg = PointImg
instance ToMessage PointImg where
    toContentType _ = C.pack "image/gif"
    toMessage _ = L.pack 
        "\x47\x49\x46\x38\x39\x61\x01\x00\x01\x00\
        \\x80\x00\x00\xFF\xFF\xFF\x00\x00\x00\x21\
        \\xF9\x04\x01\x00\x00\x00\x00\x2C\x00\x00\
        \\x00\x00\x01\x00\x01\x00\x00\x02\x02\x44\
        \\x01\x00\x3B"

-- TODO - make file config
newConf dvar = Conf { 
    port = 8080, 
    validator = Nothing, 
    logAccess = Just (DStatus.logAccess dvar logMAccess),
    timeout = 3,
    threadGroup = Nothing
    }

dstatusAction :: AcidState Stats -> TVar DStatus -> ServerPart Response
dstatusAction acidStats dvar = do 
    dstatus <- lift $ readTVarIO dvar
    stats <- query' acidStats PeekStats
    ok $ toResponse $ H.body $ do
        H.toHtml $ size stats
        H.hr
        H.toHtml $ dstatus

countAction :: AcidState Stats -> ServerPart Response
countAction acidStats = do
    c0::Int <- query' acidStats PeekStatsCount
    c1::Int <- update' acidStats IncStatsCount
    ok $ toResponse $ H.toHtml $ concat $ intersperse "," $ map show [c0, c1]

trAction :: AcidState Stats -> ServerPart Response
trAction acidStats = do
--    headers <- rqHeaders `liftM` askRq
--    lift $ putStrLn $ show headers
    referer <- (getHeader "referer") `liftM` askRq
    clientIp <- (fst.rqPeer) `liftM` askRq
    time <- lift getCurrentTime
    let visit = Visit { vzTime = time, vzClientIp = clientIp, vzReferer = referer }
    update' acidStats (IncStats $ makeIndex visit)
    ok $ toResponse PointImg

main = do
    dvar <- DStatus.new
    let conf = newConf dvar
    bracket 
        (openLocalState initialStats)
        (createCheckpointAndClose)
        (\acidStats -> simpleHTTP conf $ msum [
            dir "dstatus"     $ measure dvar $ dstatusAction acidStats dvar,
            dir "count"       $ measure dvar $ countAction acidStats,
            dir "tr"          $ measure dvar $ trAction acidStats,
            dir "favicon.ico" $ measure dvar $ serveFile (asContentType "image/vnd.microsoft.icon") "favicon.ico",
            dir "static"      $ measure dvar $ serveDirectory EnableBrowsing [] "html" 
        ])


