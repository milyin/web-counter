{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Main where

import Prelude hiding (foldl, concat)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server
import qualified DStatus
import DStatus (DStatus)
import qualified Stat
import Control.Exception.Base
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Control.Monad hiding (msum)
import Control.Monad.Trans
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
-- import Data.List
import Data.Typeable
import Stat
import Geo
import Data.Time.LocalTime
import Data.Time.Clock
import qualified Data.IxSet as IxSet
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Control.Lens
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.List (intersperse)
import Data.Time.Format
import System.Locale

data PointImg = PointImg
instance ToMessage PointImg where
    toContentType _ = C.pack "image/gif"
    toMessage _ = L.pack
        "\x47\x49\x46\x38\x39\x61\x01\x00\x01\x00\
        \\x80\x00\x00\xFF\xFF\xFF\x00\x00\x00\x21\
        \\xF9\x04\x01\x00\x00\x00\x00\x2C\x00\x00\
        \\x00\x00\x01\x00\x01\x00\x00\x02\x02\x44\
        \\x01\x00\x3B"

data Command = Help | Run

data Options = Options {
    _optCommand    :: Command,
    _optPort       :: Int,
    _optTimeout    :: Int,
    _optDbHost     :: String,
    _optDbName     :: String,
    _optDbUser     :: String,
    _optDbPassword :: String
    } 

makeLenses ''Options

defaultOptions = Options {
    _optCommand = Run,
    _optPort = 8080,
    _optTimeout = 3,
    _optDbHost = "localhost",
    _optDbName = "webcounter",
    _optDbUser = "root",
    _optDbPassword = "4321"
    }

options :: [ OptDescr (Options -> Options) ]
options = [
    Option "p" ["port"]        (reqArg optPort       "PORT")        "http listening port",
    Option "t" ["timeout"]     (reqArg optTimeout    "TIMEOUT")     "response timeout",
    Option ""  ["dbhost"]      (reqArg optDbHost     "DB_HOST")     "db host",
    Option ""  ["dbname"]      (reqArg optDbName     "DB_NAME")     "db name",
    Option ""  ["dbuser"]      (reqArg optDbUser     "DB_USER")     "db user",
    Option ""  ["dbpassword"]  (reqArg optDbPassword "DB_PASSWORD") "db password",
    Option "h" ["help"]        (noArg  Help)                        "show help"
    ]
    where
    reqArg prop = ReqArg (\s -> prop .~ read s)
    noArg cmd  = NoArg (optCommand .~ cmd)

readOptions :: [String] -> IO Options
readOptions argv = case getOpt Permute options argv of
    (o,_, [])  -> return $ foldl (flip id) defaultOptions o
    (_,_,errs) -> ioError $ userError $ concat errs 

httpConf :: Options -> TVar DStatus -> Conf
httpConf opts dvar = Conf {
    port = opts ^. optPort,
    validator = Nothing,
    logAccess = Just (DStatus.logAccess dvar logMAccess),
    timeout = opts ^. optTimeout,
    threadGroup = Nothing
    }

dstatusAction :: AcidState Stats -> TVar DStatus -> ServerPart Response
dstatusAction acidStats dvar = do
    dstatus <- lift $ readTVarIO dvar
    stats <- query' acidStats PeekStats
    ok $ toResponse $ H.body $ do
        H.toHtml (IxSet.size $ stats^.statsSet) >> H.toHtml " stat records "
        H.toHtml (length $ stats^.visitsLog) >> H.toHtml " visit records "
        H.hr
        H.toHtml dstatus

trAction :: AcidState Stats -> ServerPart Response
trAction acidStats = do
--    headers <- rqHeaders `liftM` askRq
--    lift $ putStrLn $ show headers
    ip <- (C.pack . fst . rqPeer) `liftM` askRq
    referer <- coalesce (C.pack "") $ getHeader "referer" `liftM` askRq
    user_agent <- coalesce (C.pack "") $ getHeader "user-agent" `liftM` askRq
    r <- C.pack `liftM` look "r"
    time <- liftIO getCurrentTime
    update' acidStats $ RecordVisit $ mkVisit time ip referer user_agent r
    ok $ toResponse PointImg
    where
    coalesce :: v -> ServerPart (Maybe v) -> ServerPart v
    coalesce d a = a >>= \mv -> maybe (return d) return mv

clearAction :: AcidState Stats -> ServerPart Response
clearAction acidStats = do
    update' acidStats ClearVisits
    ok $ toResponse "Data cleared"

-- jsonDataAction :: AcidState Stats -> ServerPart Response
-- jsonDataAction

showStatsAction :: AcidState Stats -> ServerPart Response
showStatsAction acidStats = do
    stats <- query' acidStats PeekStats
    ok $ toResponse $ C.concat $ intersperse (C.pack "\n") $ map toCsvRow $ toList $ stats^.visitsLog
    where 
        toCsvRow :: Visit -> B.ByteString
        toCsvRow visit = B.concat $ intersperse (C.pack ",") [ 
            C.pack $ formatTime defaultTimeLocale "%F %T" $ visit^.visitTime,
            visit^.visitIp,
            visit^.visitReferer,
            visit^.visitR,
            visit^.visitUserAgent
            ]

main = do
    opts <- getArgs >>= readOptions
    case opts ^. optCommand of
        Help -> showDaemonHelp
        Run  -> runDaemon opts

showDaemonHelp = putStrLn $ usageInfo "Available options:" options

withAcid :: (Typeable a, IsAcidic a) => a -> (AcidState a ->  IO ()) -> IO ()
withAcid init = bracket (openLocalState init) createCheckpointAndClose

withAcidDbs :: (AcidState Stats -> AcidState GeoDb -> IO ()) -> IO ()
withAcidDbs f = 
    withAcid initialStats $ \stats ->
    withAcid initialGeoDb $ \geodb -> f stats geodb

runDaemon opts = do
    dvar <- DStatus.new
    let conf = httpConf opts dvar
    withAcidDbs $ \stats geodb -> simpleHTTP conf $ mapServerPartT' (DStatus.measure dvar) $ msum [
            -- actions
            dir "dstatus"     $ dstatusAction stats dvar,
            dir "tr"          $ trAction stats,
            dir "showstats"   $ showStatsAction stats,
            dir "clear"       $ clearAction stats,
            -- static files
            dir "favicon.ico" $ serveFile (asContentType "image/vnd.microsoft.icon") "favicon.ico",
            dir "html"        $ serveDirectory DisableBrowsing ["index.html"] "html",
            dir "images"      $ serveDirectory DisableBrowsing [] "images",
            dir "css"         $ serveDirectory DisableBrowsing [] "css",
            nullDir           >> serveFile (asContentType "text/html") "html/index.html"
        ]

