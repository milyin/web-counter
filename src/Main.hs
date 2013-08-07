module Main where

import Happstack.Server
import Data.Foldable
import qualified DStatus;

-- TODO - make file config
newConf dvar = Conf { 
    port = 8080, 
    validator = Nothing, 
    logAccess = Just (DStatus.logAccess dvar logMAccess),
    timeout = 3,
    threadGroup = Nothing
    }

main = do
    dvar <- DStatus.new
    simpleHTTP (newConf dvar) $ msum [
        dir "dstatus" $ DStatus.measure dvar $ DStatus.display dvar
        ]


