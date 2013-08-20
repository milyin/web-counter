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
import Stat
import Data.Time.LocalTime
import Data.Time.Clock
import Data.IxSet
import Data.Maybe
import System.Console.GetOpt
import Data.Lens.Common
import Data.Lens.Template (makeLens)
import System.Environment

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
    _optCommand  :: Command,
    _optPort     :: Int,
    _optTimeout  :: Int
    } 

$(makeLens ''Options)

defaultOptions = Options {
    _optCommand = Run,
    _optPort = 8080,
    _optTimeout = 3
    }

options :: [ OptDescr (Options -> Options) ]
options = [
    Option ['p'] ["port"]    (reqArg optPort    "PORT")    "http listening port",
    Option ['t'] ["timeout"] (reqArg optTimeout "TIMEOUT") "response timeout",
    Option ['h'] ["help"]    (noArg  Help)                 "show help"
    ]
    where
    reqArg prop ad = ReqArg (\s -> setL prop $ read s) ad
    noArg cmd  = NoArg (setL optCommand cmd)

readOptions :: [String] -> IO Options
readOptions argv = case getOpt Permute options argv of
    (o,_, [])  -> return $ foldl (flip id) defaultOptions o
    (_,_,errs) -> ioError $ userError $ concat errs 

newConf :: Options -> TVar DStatus -> Conf
newConf opts dvar = Conf {
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

runDaemon opts = do
    dvar <- DStatus.new
    let conf = newConf opts dvar
    bracket
        (openLocalState initialStats)
        (createCheckpointAndClose)
        (\acidStats -> simpleHTTP conf $ mapServerPartT' (DStatus.measure dvar) $ msum [
            dir "dstatus"     $ dstatusAction acidStats dvar,
            dir "tr"          $ trAction acidStats,
            dir "showstats"   $ showStatsAction acidStats,
            dir "favicon.ico" $ serveFile (asContentType "image/vnd.microsoft.icon") "favicon.ico",
            dir "static"      $ serveDirectory EnableBrowsing [] "html"
        ])


