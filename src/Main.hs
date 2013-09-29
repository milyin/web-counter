{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}

module Main where

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
import Control.Monad
import Control.Monad.Trans
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Data.Acid
import Data.Acid.Advanced
import Data.Acid.Local
import Data.List
import Data.Typeable
import Stat
import Geo
import Data.Time.LocalTime
import Data.Time.Clock
import Data.IxSet
import Data.Maybe
import System.Console.GetOpt
import System.Environment
import Control.Lens
import Database.MySQL.Simple

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
    Option ['p'] ["port"]        (reqArg optPort       "PORT")        "http listening port",
    Option ['t'] ["timeout"]     (reqArg optTimeout    "TIMEOUT")     "response timeout",
    Option []    ["dbhost"]      (reqArg optDbHost     "DB_HOST")     "db host",
    Option []    ["dbname"]      (reqArg optDbName     "DB_NAME")     "db name",
    Option []    ["dbuser"]      (reqArg optDbUser     "DB_USER")     "db user",
    Option []    ["dbpassword"]  (reqArg optDbPassword "DB_PASSWORD") "db password",
    Option ['h'] ["help"]        (noArg  Help)                        "show help"
    ]
    where
    reqArg prop ad = ReqArg (\s -> prop .~ read s) ad
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
        H.toHtml (size stats) >> H.toHtml " records"
        H.hr
        H.toHtml $ dstatus

trAction :: AcidState Stats -> ServerPart Response
trAction acidStats = do
--    headers <- rqHeaders `liftM` askRq
--    lift $ putStrLn $ show headers
    referer <- require $ (getHeader "referer") `liftM` askRq
    clientIp <- (fst.rqPeer) `liftM` askRq
    time <- liftIO getCurrentTime
    let visit = Visit { vzTime = time, vzClientIp = clientIp, vzReferer = referer }
    update' acidStats (IncStats $ makeIndex visit)
    ok $ toResponse PointImg
    where
    require :: ServerPart (Maybe v) -> ServerPart v
    require a = a >>= \mv -> maybe interrupt return mv
    interrupt = finishWith $ toResponse PointImg

-- jsonDataAction :: AcidState Stats -> ServerPart Response
-- jsonDataAction

showStatsAction :: AcidState Stats -> ServerPart Response
showStatsAction acidStats = do
    stats <- query' acidStats PeekStats
    ok $ toResponse $ H.body $ do
        H.toHtml $ fmap (\s -> H.toHtml (show s) >> H.br) $ toList stats

main = do
    opts <- getArgs >>= readOptions
    case (opts ^. optCommand) of
        Help -> showDaemonHelp
        Run  -> runDaemon opts

showDaemonHelp = putStrLn $ usageInfo "Available options:" options

withAcid :: (Typeable a, IsAcidic a) => a -> (AcidState a ->  IO ()) -> IO ()
withAcid init = bracket (openLocalState init) createCheckpointAndClose

withAcidDbs :: (AcidState Stats -> AcidState GeoDb -> IO ()) -> IO ()
withAcidDbs f = 
    withAcid initialStats $ \stats ->
    withAcid initialGeoDb $ \geodb -> f stats geodb

makeConnectInfo opts = defaultConnectInfo {
    connectHost     = opts ^. optDbHost,
    connectUser     = opts ^. optDbUser,
    connectDatabase = opts ^. optDbName,
    connectPassword = opts ^. optDbPassword
    }

runDaemon opts = do
    dvar <- DStatus.new
    let conf = httpConf opts dvar
    let mysql_conn = connect $ makeConnectInfo opts
    withAcidDbs $ \stats geodb -> simpleHTTP conf $ mapServerPartT' (DStatus.measure dvar) $ msum [
            dir "dstatus"     $ dstatusAction stats dvar,
            dir "tr"          $ trAction stats,
            dir "showstats"   $ showStatsAction stats,
            dir "favicon.ico" $ serveFile (asContentType "image/vnd.microsoft.icon") "favicon.ico",
            dir "static"      $ serveDirectory EnableBrowsing [] "html"
        ]

