module Tests.Data.CapNProto.Untyped where

import Test.HUnit (assertEqual, Test(TestCase, TestList))
import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Tests.Util
import Control.Monad.Quota
import Data.CapNProto.Untyped

untypedTests = testGroup "Untyped Tests" $ hUnitTestToTests $ TestList $ map tst
    [ ( TestMessage { schemaName = "misc", typeName = "A", constName = "misc"}
      , 128
      , \(PtrStruct _) -> return ()
      , ((), Quota 72) -- the quota value is almost certainly wrong; I picked
                       -- an arbitrary value, which I'll fix once the test is
                       -- actually getting that far, but right now it's just
                       -- hanging.
      )
    ]
  where
    tst (testMessage, quota, m, expected) = TestCase $ do
        msg <- getTestMessage testMessage quota
        actual <- runQuotaT (rootPtr msg >>= m) (Quota quota)
        assertEqual (show testMessage) actual expected
