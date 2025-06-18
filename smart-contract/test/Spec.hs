{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main(main) where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Spec.Refi qualified
import Spec.StakeReward qualified

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "use cases" [
    Spec.Refi.tests,
    Spec.StakeReward.tests
  ]