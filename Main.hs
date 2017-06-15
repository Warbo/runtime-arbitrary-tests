{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List
import           Language.Eval
import           Test.RuntimeArbitrary
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty (defaultMain, testGroup)
import           Test.Tasty.QuickCheck (testProperty)

main = defaultMain $ testGroup "All tests" $ concat
  [haveOrds, noOrds, haveArbs, haveGens, gensWork]

-- Tests

haveOrds = map f ["Int", "[Int]", "Foo", "[Foo]"]
  where f t = testProperty ("Found Ord instance for " ++ t)
                           (givesTrue (isInstance' "Ord" t))

noOrds = map f ["(Int -> Int)", "[Int -> Int]"]
  where f t = testProperty ("No Ord instance for " ++ t)
                           (givesTrue (notInstance' "Ord" t))

haveArbs = map f ["Int", "Foo"]
  where f t = testProperty ("Found Arbitrary instance for " ++ t)
                           (givesTrue (isInstance' "Arbitrary" t))

haveGens = map f  [("Int", intGens), ("Foo", fooGens)]
  where f (t, g) = testProperty ("Have generator for " ++ t)
                                (givesTrue ("show" $$ notNull' g))

gensWork = map f [("Int", intGens), ("Foo", fooGens)]
  where f (t, g) = testProperty ("Generator works for " ++ t)
                                (givesTrue (test' g))

-- Generators

intGens = arbGen "Int"

fooGens = withMods ["Test.TestInstances"] . withPkgs ["runtime-arbitrary"] $
          arbGen "Foo"

-- Run nix-eval and check the result (Nothing for expected failure)

givesTrue :: Expr -> Property
givesTrue i = once $ counterexample (show i) $ ioProperty $ do
  result <- eval i
  return (Just "True" === result)

-- Quoted functions

isInstance'  cls typ = ifCxt' cls typ true'  false'
notInstance' cls typ = ifCxt' cls typ false' true'

ifCxt' cls typ thn els =
  withInstances                                                                .
  withPkgs ["ifcxt", "QuickCheck", "runtime-arbitrary"]                        .
  withMods ["IfCxt", "Data.Typeable", "Test.QuickCheck", "Test.TestInstances"] $
  expr'
  where expr'  = (("ifCxt" $$ proxy') $$ thn) $$ els
        proxy' = raw $ "(Proxy :: Proxy (" ++ cls ++ " " ++ typ ++ "))"

true'  = asString "True"
false' = asString "False"

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
               qualified "Test.RuntimeArbitrary" "getArbGen"

arbGen t = withInstances $ getArbGen' $$ argOfType' t

argOfType' t = raw $ "[undefined :: (" ++ t ++ ")]"

notNull' x = "not" $$ "null" $$ x

withTH = withFlags ["-XTemplateHaskell"] . withPkgs ["template-haskell"]

withIfCxt = withFlags ["-XFlexibleInstances",
                       "-XKindSignatures",
                       "-XScopedTypeVariables",
                       "-XMultiParamTypeClasses",
                       "-XFlexibleContexts"] .
            withPkgs  ["ifcxt"] .
            withMods  ["IfCxt"]

withInstances = withIfCxt                                   .
                withMods ["Test.QuickCheck"]                .
                withMods ["Data.Typeable"]                  .
                withPreamble "mkIfCxtInstances ''Arbitrary" .
                withPreamble "mkIfCxtInstances ''Ord"       .
                withTH
