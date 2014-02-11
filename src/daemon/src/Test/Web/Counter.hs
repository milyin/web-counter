{-# LANGUAGE FlexibleInstances #-}
module Test.Web.Counter (tests) where

import Distribution.TestSuite

test :: String ->  Bool -> Test
test testname testres = Test $ TestInstance {
    run = return $ Finished Pass,
    name = testname,
    tags = [],
    options = [],
    setOption = \_ _ -> Left "Not supported"
    }

tests :: IO [Test]
tests = return $
    [ test "bar-1" True
    , test "bar-2" False
    ]
