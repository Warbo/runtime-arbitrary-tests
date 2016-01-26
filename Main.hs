{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Language.Eval
import           RuntimeArbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

main = defaultMain $ testGroup "All tests" [
    testProperty "Found Int's Ord instance"       haveIntOrd
  , testProperty "Found Int's Arbitrary instance" haveIntArbitrary
  , testProperty "Found Foo's Ord instance"       haveFooOrd
  , testProperty "Found Foo's Arbitrary instance" haveFooArbitrary
  , testProperty "Have an Int generator"          haveIntGen
  , testProperty "Int generator works"            intGenWorks
  , testProperty "Have a Foo generator"           haveFooGen
  , testProperty "Foo generator works"            fooGenWorks
  ]

intGens = arbGen "Int"

fooGens = withMods ["TestInstances"] . withPkgs ["runtime-arbitrary"] $
          arbGen "Foo"

haveIntOrd       = givesTrue (isInstance' "Ord"       "Int")
haveIntArbitrary = givesTrue (isInstance' "Arbitrary" "Int")

haveFooOrd       = givesTrue (isInstance' "Ord"       "Foo")
haveFooArbitrary = givesTrue (isInstance' "Arbitrary" "Foo")

haveIntGen = givesTrue ("show" $$ notNull' intGens)

haveFooGen = givesTrue ("show" $$ notNull' fooGens)

intGenWorks = givesTrue (test' intGens)

fooGenWorks = givesTrue (test' fooGens)

-- Run nix-eval and check the result (Nothing for expected failure)

givesTrue :: Expr -> Property
givesTrue i = once $ counterexample (show i) $ ioProperty $ do
  result <- eval i
  return (Just "True" === result)

-- Quoted functions

isInstance' c t =
  withInstances                                                           .
  withPkgs ["ifcxt", "QuickCheck", "runtime-arbitrary"]                   .
  withMods ["IfCxt", "Data.Typeable", "Test.QuickCheck", "TestInstances"] $
  raw ("(ifCxt (Proxy :: Proxy (" ++ c ++ " " ++ t ++ ")) \"True\" \"False\")")

forAll' = withPkgs ["QuickCheck"] $ qualified "Test.QuickCheck" "forAll"

oneof' = withPkgs ["QuickCheck"] $ qualified "Test.QuickCheck" "oneof"

quickCheck' = withPkgs ["QuickCheck"] $
              qualified "Test.QuickCheck" "quickCheckWithResult" $$
              "(Test.QuickCheck.stdArgs { chatty = False })"

testWith' gen tst = quickCheck' $$ (forAll' $$ oneof' $$ gen) $$ tst

test' gen = show' (testWith' gen constTrue')

constTrue' = "const" $$ "True"

isSuccess' = qualified "Test.QuickCheck.Test" "isSuccess"

unsafePerformIO' = qualified "System.IO.Unsafe" "unsafePerformIO"

show' x = "show" $$ isSuccess' $$ unsafePerformIO' $$ x

getArbGen' = withPkgs ["runtime-arbitrary"] $
               qualified "RuntimeArbitrary" "getArbGen"

arbGen t = withInstances $ getArbGen' $$ argOfType' t

argOfType' t = raw $ "[undefined :: (" ++ t ++ ")]"

notNull' x = "not" $$ "null" $$ x

withTH = withFlags ["-XTemplateHaskell"] . withPkgs ["template-haskell"]

withIfCxt = withFlags ["-XFlexibleInstances",
                       "-XKindSignatures",
                       "-XScopedTypeVariables",
                       "-XMultiParamTypeClasses"] .
            withPkgs  ["ifcxt"] .
            withMods  ["IfCxt"]

withInstances = withIfCxt                                   .
                withMods ["Test.QuickCheck"]                .
                withMods ["Data.Typeable"]                  .
                withPreamble "mkIfCxtInstances ''Arbitrary" .
                withPreamble "mkIfCxtInstances ''Ord"       .
                withTH
