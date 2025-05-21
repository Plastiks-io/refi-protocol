{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool)
import Plutus.V2.Ledger.Api 
import Plutus.V2.Ledger.Contexts 
import Plutus.V1.Ledger.Interval (always)
import qualified PlutusTx.AssocMap as AssocMap
import qualified Refi.Validator as Validator

main :: IO ()
main = defaultMain tests

-- Helper function to create mock context (unchanged)
mkContext :: PubKeyHash -> Bool -> ScriptContext
mkContext pkh signed =
  ScriptContext
    { scriptContextTxInfo = TxInfo
        { txInfoInputs = []
        , txInfoReferenceInputs = []
        , txInfoOutputs = []
        , txInfoFee = mempty
        , txInfoMint = mempty
        , txInfoDCert = []
        , txInfoWdrl = AssocMap.empty
        , txInfoValidRange = always
        , txInfoSignatories = if signed then [pkh] else []
        , txInfoRedeemers = AssocMap.empty
        , txInfoData = AssocMap.empty
        , txInfoId = TxId "dummy_tx_id"
        }
    , scriptContextPurpose = Spending (TxOutRef (TxId "dummy_tx_id") 0)
    }

-- Test constants updated for scaling
testAdmin :: PubKeyHash
testAdmin = PubKeyHash "admin"

testPrePkh :: PubKeyHash
testPrePkh = PubKeyHash "pre_pkh"

testPreSkh :: PubKeyHash
testPreSkh = PubKeyHash "pre_skh"

-- Default datum with scaled progress (5000 = 50.00%)
defaultDatum :: Validator.PlastiksDatum
defaultDatum = Validator.PlastiksDatum
  { Validator.preId = "pre123"
  , Validator.roadmapId = "roadmap456"
  , Validator.roadmapName = "Test Roadmap"
  , Validator.roadmapDescription = "Test Description"
  , Validator.progress = 5000  -- Changed from 50 to 5000 (50.00%)
  , Validator.adminPkh = testAdmin
  , Validator.prePkh = testPrePkh
  , Validator.preSkh = testPreSkh 
  , Validator.totalPlasticCredits = 1000
  , Validator.soldPlasticCredits = 500
  , Validator.totalPlasticTokens = 2000
  , Validator.sentPlasticTokens = 1000
  , Validator.totalPlastic = 10000
  , Validator.recoverPlastic = 5000
  , Validator.createdAt = "05-13-2025"
  }

-- Updated test cases with scaled values
tests :: TestTree
tests = testGroup "Plastiks Validator Tests"
  [ testGroup "UpdateProgress Redeemer"
      [ testCase "Valid update by admin" $ do
          let redeemer = Validator.UpdateProgress 7500  -- 75.00%
              ctx = mkContext testAdmin True
          assertBool "Should allow valid progress update" $
            Validator.validate defaultDatum redeemer ctx

      , testCase "Update without admin signature" $ do
          let redeemer = Validator.UpdateProgress 7500
              ctx = mkContext testAdmin False
          assertBool "Should prevent unauthorized update" $
            not (Validator.validate defaultDatum redeemer ctx)

      , testCase "Invalid lower progress update" $ do
          let redeemer = Validator.UpdateProgress 4000  -- 40.00%
              ctx = mkContext testAdmin True
          assertBool "Should prevent decreasing progress" $
            not (Validator.validate defaultDatum redeemer ctx)

      , testCase "Invalid progress over 100%" $ do  -- Updated test name
          let redeemer = Validator.UpdateProgress 10001  -- 100.01%
              ctx = mkContext testAdmin True
          assertBool "Should prevent progress > 100%" $
            not (Validator.validate defaultDatum redeemer ctx)

      , testCase "Update to exactly 100%" $ do  -- Updated test name
          let redeemer = Validator.UpdateProgress 10000  -- 100.00%
              ctx = mkContext testAdmin True
          assertBool "Should allow progress to 100%" $
            Validator.validate defaultDatum redeemer ctx
      ]

  , testGroup "Release Redeemer"
      [ testCase "Valid release with 100% progress" $ do
          let datum = defaultDatum { Validator.progress = 10000 }  -- 100.00%
              redeemer = Validator.Release
              ctx = mkContext testAdmin True
          assertBool "Should allow valid release" $
            Validator.validate datum redeemer ctx

      , testCase "Release without admin signature" $ do
          let datum = defaultDatum { Validator.progress = 10000 }
              redeemer = Validator.Release
              ctx = mkContext testAdmin False
          assertBool "Should prevent unauthorized release" $
            not (Validator.validate datum redeemer ctx)

      , testCase "Release with incomplete progress" $ do
          let datum = defaultDatum { Validator.progress = 9900 }  -- 99.00%
              redeemer = Validator.Release
              ctx = mkContext testAdmin True
          assertBool "Should prevent early release" $
            not (Validator.validate datum redeemer ctx)
      ]

    , testGroup "Archived Redeemer"
        [ testCase "Archived without admin signature" $ do
            let datum = defaultDatum
                redeemer = Validator.Archived
                ctx = mkContext testAdmin False
            assertBool "Should prevent unauthorized archived" $
                not (Validator.validate datum redeemer ctx)

        , testCase "Archived with admin signature" $ do
            let datum = defaultDatum 
                redeemer = Validator.Archived
                ctx = mkContext testAdmin True
            assertBool "Should Archived roadmap" $
                Validator.validate datum redeemer ctx
        ]

  ]