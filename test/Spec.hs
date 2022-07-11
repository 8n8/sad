module Main (main) where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

main :: IO ()
main =
    Tasty.defaultMain $
    HUnit.testCase "greet" $ do
        2 + 2 HUnit.@?= 4
