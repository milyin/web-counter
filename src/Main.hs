{-# LANGUAGE ScopedTypeVariables #-}

module Main where

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

-- TODO - make file config
newConf dvar = Conf { 
    port = 8080, 
    validator = Nothing, 
    logAccess = Just (DStatus.logAccess dvar logMAccess),
    timeout = 3,
    threadGroup = Nothing
    }

dstatusAction :: TVar DStatus -> ServerPart Response
dstatusAction dvar = do 
    dstatus <- lift $ readTVarIO dvar 
    ok $ toResponse $ H.toHtml $ dstatus

countAction :: AcidState Stats -> ServerPart Response
countAction acidStats = do
    c0::Int <- query' acidStats PeekStat
    c1::Int <- update' acidStats IncStat
    ok $ toResponse $ H.toHtml $ concat $ intersperse "," $ map show [c0, c1]

main = do
    dvar <- DStatus.new
    let conf = newConf dvar
    bracket 
        (openLocalState initialStats)
        (createCheckpointAndClose)
        (\acidStats -> simpleHTTP conf $ msum [
            dir "dstatus" $ DStatus.measure dvar $ dstatusAction dvar,
            dir "count"   $ DStatus.measure dvar $ countAction acidStats
        ])


