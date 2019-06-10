{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-missing-signatures #-}

import GHC.Generics
import Data.SafeCopy
import Data.SafeCopy.Internal
import Data.Serialize (runGet, runPut, Serialize)
import Data.Serialize.Get
import Data.Serialize.Put
import Text.Printf
import Test.HUnit (Test(..), assertEqual, runTestTT)
import Generic.Data as G hiding (unpack)

-- Debugging
import Data.Typeable hiding (Proxy)
import Debug.Trace
import Data.ByteString (ByteString, unpack)

-- Test types
data Foo = Foo Int Char deriving (Generic, Serialize, Show, Eq)
data Bar = Bar Float Foo deriving (Generic, Serialize, Show, Eq)
data Baz = Baz1 Int | Baz2 Bool deriving (Generic, Serialize, Show, Eq)

safePutTest :: forall a. (SafeCopy a, Generic a, GPutCopy (Rep a) DatatypeInfo, GConstructors (Rep a)) => a -> Put
safePutTest a =
  case runPut p1 == runPut p2 of
    True -> p1
    False -> trace (" custom: " ++ showBytes (runPut p1) ++ "\n generic: " ++ showBytes (runPut p2)) p1
  where
    p1 = safePut a
    p2 = safePutGeneric a

----------------------------------------------

roundTrip :: forall a. (SafeCopy a, Typeable a, Eq a, Show a) => a -> Test
roundTrip x = do
  -- putStrLn ("\n========== " ++ show x ++ " :: " ++ show (typeRep (Proxy :: Proxy a)) ++ " ==========")
  let d = runPut (safePut x) -- Use custom putCopy/getCopy implementation if present
      a :: Either String a
      a = runGet safeGet d
  TestCase (assertEqual ("roundTrip " ++ show x ++ " :: " ++ show (typeRep (Proxy :: Proxy a))) (Right x) a)

-- Test whether two values of different types have the same encoded
-- representation.  This is used here on types of similar shape to
-- test whether the generic SafeCopy instance matches the template
-- haskell instance.
compareBytes ::
  forall expected actual. (SafeCopy expected, Typeable expected,
                           SafeCopy actual, Typeable actual)
  => expected -> actual -> Test
compareBytes e a =
  TestCase (assertEqual ("compareBytes " ++ show (typeRep (Proxy :: Proxy expected)) ++ " " ++
                                            show (typeRep (Proxy :: Proxy actual)))
              (showBytes (runPut $ safePut e))
              (showBytes (runPut $ safePut a)))

showBytes :: ByteString -> String
showBytes b = mconcat (fmap (printf "%x") (unpack b))

-----------------------------
-- Test Types and Values
-----------------------------

foo = Foo maxBound 'x'
bar = Bar 1.5 foo
baz1 = Baz1 3
baz2 = Baz2 True

-- These instances will use the generic putCopy and getCopy
instance SafeCopy Foo where version = 3; kind = base
instance SafeCopy Bar where version = 4; kind = base
instance SafeCopy Baz where version = 5; kind = base

-- Copies of the types above with generated SafeCopy instances
data FooTH = FooTH Int Char deriving (Generic, Serialize, Show, Eq)
data BarTH = BarTH Float FooTH deriving (Generic, Serialize, Show, Eq)
data BazTH = Baz1TH Int | Baz2TH Bool deriving (Generic, Serialize, Show, Eq)

fooTH = FooTH maxBound 'x'
barTH = BarTH 1.5 fooTH
baz1TH = Baz1TH 3
baz2TH = Baz2TH True

-- For comparison, these instances have the generated implementations
-- of putCopy and getCopy
#if 0
$(deriveSafeCopy 3 'base ''FooTH)
$(deriveSafeCopy 4 'base ''BarTH)
$(deriveSafeCopy 5 'base ''BazTH)
#else
instance SafeCopy FooTH where
      putCopy (FooTH a1_aeVVN a2_aeVVO)
        = contain
            (do safePut_Int_aeVVP <- getSafePut
                safePut_Char_aeVVQ <- getSafePut
                safePut_Int_aeVVP a1_aeVVN
                safePut_Char_aeVVQ a2_aeVVO
                return ())
      getCopy
        = contain
            ((Data.Serialize.Get.label "Main.FooTH:")
               (do safeGet_Int_aeVVR <- getSafeGet
                   safeGet_Char_aeVVS <- getSafeGet
                   ((return FooTH <*> safeGet_Int_aeVVR) <*> safeGet_Char_aeVVS)))
      version = 3
      kind = base
      errorTypeName _ = "Main.FooTH"

instance SafeCopy BarTH where
      putCopy (BarTH a1_aeVXE a2_aeVXF)
        = contain
            (do safePut_Float_aeVXG <- getSafePut
                safePut_FooTH_aeVXH <- getSafePut
                safePut_Float_aeVXG a1_aeVXE
                safePut_FooTH_aeVXH a2_aeVXF
                return ())
      getCopy
        = contain
            ((Data.Serialize.Get.label "Main.BarTH:")
               (do safeGet_Float_aeVXI <- getSafeGet
                   safeGet_FooTH_aeVXJ <- getSafeGet
                   ((return BarTH <*> safeGet_Float_aeVXI) <*> safeGet_FooTH_aeVXJ)))
      version = 4
      kind = base
      errorTypeName _ = "Main.BarTH"

instance SafeCopy BazTH where
      putCopy (Baz1TH a1_aeVZv)
        = contain
            (do Data.Serialize.Put.putWord8 0
                safePut_Int_aeVZw <- getSafePut
                safePut_Int_aeVZw a1_aeVZv
                return ())
      putCopy (Baz2TH a1_aeVZx)
        = contain
            (do Data.Serialize.Put.putWord8 1
                safePut_Bool_aeVZy <- getSafePut
                safePut_Bool_aeVZy a1_aeVZx
                return ())
      getCopy
        = contain
            ((Data.Serialize.Get.label "Main.BazTH:")
               (do tag_aeVZz <- Data.Serialize.Get.getWord8
                   case tag_aeVZz of
                     0 -> do safeGet_Int_aeVZA <- getSafeGet
                             (return Baz1TH <*> safeGet_Int_aeVZA)
                     1 -> do safeGet_Bool_aeVZB <- getSafeGet
                             (return Baz2TH <*> safeGet_Bool_aeVZB)
                     _ -> fail
                            ("Could not identify tag \""
                               ++
                                 (show tag_aeVZz
                                    ++
                                      "\" for type \"Main.BazTH\" that has only 2 constructors.  Maybe your data is corrupted?"))))
      version = 5
      kind = base
      errorTypeName _ = "Main.BazTH"
#endif

----------------------------------------------
-- Demonstration of the ordering issue
----------------------------------------------

data T1 = T1 Char T2 T3
data T2 = T2 Char
data T3 = T3 Char

t1 = T1 'a' (T2 'b') (T3 'c')

$(deriveSafeCopy 3 'base ''T1)
$(deriveSafeCopy 4 'base ''T2)
$(deriveSafeCopy 5 'base ''T3)

data T1G = T1G Char T2G T3G deriving Generic
data T2G = T2G Char deriving Generic
data T3G = T3G Char deriving Generic

t1g = T1G 'a' (T2G 'b') (T3G 'c')

instance SafeCopy T1G where version = 3; kind = base
instance SafeCopy T2G where version = 4; kind = base
instance SafeCopy T3G where version = 5; kind = base

orderTests :: Test
orderTests =
  let expected = ("0003" <> "0000" <> "0004" <> "0005" <> "61" <> "0000" <> "62" <> "0000" <> "63")
               --   T1       Char       T2        T3       'a'     Char     'b'      Char      'c'
  in
  TestList
     [ TestCase (assertEqual "actual template haskell safeput output" expected (showBytes (runPut (safePut t1))))
     , TestCase (assertEqual "what the new implementation does"       expected (showBytes (runPut (safePut t1g))))
     ]

main = do
  runTestTT
    (TestList
      [ roundTrip ()
      , roundTrip ("hello" :: String)
      , roundTrip foo
      , roundTrip fooTH
      , roundTrip bar
      , roundTrip barTH
      , roundTrip baz1
      , roundTrip baz1TH
      , roundTrip baz2
      , roundTrip baz2TH
      , roundTrip (Just 'x')
      , roundTrip (Nothing :: Maybe Char)
      , roundTrip ('a', (123 :: Int), ("hello" :: String))
      , compareBytes fooTH foo
      , compareBytes barTH bar
      , compareBytes baz1TH baz1
      , compareBytes baz2TH baz2
      , orderTests
      ])
