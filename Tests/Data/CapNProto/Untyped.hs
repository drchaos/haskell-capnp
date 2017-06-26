{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Tests.Data.CapNProto.Untyped where


import qualified Data.ByteString as BS
import Data.ReinterpretCast (wordToDouble)
import Text.Heredoc (here, there)
import Prelude hiding (length)

import Control.Monad (void, forM_, when)

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Tests.Util
import Control.Monad.Quota
import Data.CapNProto.Untyped

import qualified Data.Vector as B
import qualified Data.Vector.Unboxed as U

aircraftSchema = [there|testdata/aircraft.capnp|]

untypedTests = testGroup "Untyped Tests" $ hUnitTestToTests $ TestList $ map tst
    [ ( [here|@0xaa6fda3b444d262c;
               struct A {
                  a @0 :UInt64;
                  b @1 :Bool;
               }
            |]
      , "A"
      , "( a = 72, b = true )"
      , 128
      , \(Just (PtrStruct root)) -> do
            s <- get root
            words <- dataSection s
            2 <- length words
            72 <- get =<< index 0 words
            1 <- get =<< index 1 words
            ptrs <- ptrSection s
            0 <- length ptrs
            return ()
      , ((), Quota 122)
      )
    , ( aircraftSchema
      , "Aircraft"
      , [here|(f16 = (base = (
            name = "bob",
            homes = [],
            rating = 7,
            canFly = true,
            capacity = 5173,
            maxSpeed = 12.0,
        )))|]
      , 128
      , \(Just (PtrStruct root)) -> do
            s <- get root
            aircraftWords <- dataSection s
            -- Aircraft just has the union tag, nothing else in it's data
            -- section.
            1 <- length aircraftWords
            3 <- get =<< index 0 aircraftWords -- tag for F16
            aircraftPtrSec <- ptrSection s
            1 <- length aircraftPtrSec
            Just (PtrStruct f16Ptr) <- get =<< index 0 aircraftPtrSec
            f16 <- get f16Ptr
            0 <- length =<< dataSection f16
            f16PtrSec <- ptrSection f16
            1 <- length f16PtrSec
            Just (PtrStruct basePtr) <- get =<< index 0 f16PtrSec
            base <- get basePtr
            baseWords <- dataSection base
            basePtrSec <- ptrSection base
            4 <- length baseWords -- Except canFly, each field is 1 word, and
                                  -- canFly is aligned such that it ends up
                                  -- consuming a whole word.
            2 <- length basePtrSec -- name, homes

            -- Walk the data section:
            7 <- get =<< index 0 baseWords -- rating
            1 <- get =<< index 1 baseWords -- canFly
            5173 <- get =<< index 2 baseWords -- capacity
            12.0 <- wordToDouble <$> (get =<< index 3 baseWords)

            -- ...and the pointer section:
            -- FIXME: we don't implement get for list pointers yet
            {-
            Just (PtrList namePtr) <- get =<< index 0 basePtrSec
            List8 name <- get namePtr
            3 <- length name
            forM_ (zip [0..2] (BS.unpack "bob")) $ \(i, c) -> do
                c' <- get =<< index i name
                when (c /= c') $ error (show c ++ " /= " ++ show c')
            Just (PtrList homesPtr) <- get =<< index 1 basePtrSec
            ListPtr homes <- get homesPtr
            0 <- length homes
            -}

            return ()
      , ((), Quota 110)
      )
    ]
  where
    tst (schema, typename, value, quota, m, expected) = TestCase $ do
        let testMessage = TestMessage schema typename value
        msg <- getTestMessage testMessage quota
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show testMessage) expected actual