{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Language.Eval
import           RuntimeArbitrary
import           Test.QuickCheck
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

main = defaultMain $ testGroup "All tests" [
    testProperty "Have an Int generator" haveIntGen
  , testProperty "Int generator works"   intGenWorks
  , testProperty "Have a Foo generator"  haveFooGen
  , testProperty "Foo generator works"   fooGenWorks
  ]

intGens = arbGen "Int"

fooGens = withMods ["TestInstances"] . withPkgs ["runtime-arbitrary"] $
          arbGen "Foo"

haveIntGen = run ("show" $$ notNull' intGens) givesTrue

haveFooGen = run ("show" $$ notNull' fooGens) givesTrue

intGenWorks = run (test' intGens) givesTrue

fooGenWorks = run (test' fooGens) givesTrue

-- Run nix-eval and check the result (Nothing for expected failure)

run :: Expr -> (Maybe String -> Property) -> Property
run i p = once $ ioProperty $ do
  result <- eval i
  return (p result)

givesTrue = (Just "True" ===)

-- Quoted functions

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

withIfCxt = withFlags ["-XFlexibleInstances"] .
            withPkgs  ["ifcxt"] .
            withMods  ["IfCxt"]

withInstances = withIfCxt .
                withMods ["Test.QuickCheck"] .
                withPreamble "mkIfCxtInstances ''Arbitrary" .
                withTH

{-
RankNTypes, FlexibleInstances, FlexibleContexts, KindSignatures, ScopedTypeVariables, TemplateHaskell, ConstraintKinds, TemplateHaskell

module ArbInstances where

import IfCxt
import Test.QuickCheck

mkIfCxtInstances ''Arbitrary

-}
