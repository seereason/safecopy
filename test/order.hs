-- | Demonstration of the safecopy output ordering issue

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.ByteString (ByteString, unpack)
import Data.Serialize.Get
import Data.Serialize.Put
import Data.SafeCopy
import Test.HUnit
import Text.Printf

data T1 = T1 Char T2 T3
data T2 = T2 Char
data T3 = T3 Char

t1 :: T1
t1 = T1 'a' (T2 'b') (T3 'c')

$(deriveSafeCopy 3 'base ''T1)
$(deriveSafeCopy 4 'base ''T2)
$(deriveSafeCopy 5 'base ''T3)

showBytes :: ByteString -> String
showBytes b = mconcat (fmap (printf "%x") (unpack b))

main :: IO ()
main =
  let current =
            ("0003" <> "0000" <> "0004" <> "0005" <> "61" <> "0000" <> "62" <> "0000" <> "63")
          --   T1       Char       T2        T3       'a'     Char     'b'      Char      'c'
      better =
            ("0003" <> "0000" <> "61" <> "0004" <> "0000" <> "62" <> "0005" <> "0000" <> "63")
          --   T1       Char      'a'      T2       Char     'b'       T3      Char      'c'
  in
  runTestTT
    (TestList
     [ TestCase (assertEqual "actual template haskell safeput" current (showBytes (runPut (safePut t1))))
     , TestCase (assertEqual "what I would have expected"      better  (showBytes (runPut (safePut t1))))
     ]) >>= putStrLn . show
