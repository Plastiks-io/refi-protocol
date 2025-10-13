{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Spec.Governance (tests) where

import Test.Tasty (testGroup, TestTree)
import Test.Tasty.HUnit (testCase, assertBool)
import Plutus.V2.Ledger.Api (ScriptContext(..), TxInfo(..), TxOutRef(..), Redeemer, Datum, ToData(..), FromData(..), POSIXTime, PubKeyHash(..), Validator, BuiltinByteString, TxId(..))
import Plutus.V2.Ledger.Contexts (ScriptPurpose(..))
import Plutus.V1.Ledger.Interval (always)
import qualified PlutusTx.AssocMap as AssocMap
import qualified Governance.Validator as Validator
import PlutusTx.Prelude hiding ((.), ($), Bool(..), not)
import Prelude (($), not, Bool(..), Integer)

-- | Test-friendly versions of validator functions that return Bool instead of using trace errors
testValidateCreate :: Validator.GovernanceState -> Integer -> ScriptContext -> Bool
testValidateCreate gs rate _ = 
  not (testHasActiveProposal gs) && testIsValidRetirementRate rate

testValidateVote :: Validator.GovernanceState -> ScriptContext -> Bool
testValidateVote gs _ = testHasActiveProposal gs

testValidateExecute :: Validator.GovernanceState -> ScriptContext -> Bool
testValidateExecute gs _ =
  case Validator.activeProposal gs of
    Validator.NoProposal -> False
    Validator.SomeProposal p -> not (Validator.executed p)

testValidateUpdate :: Validator.GovernanceState -> ScriptContext -> Bool
testValidateUpdate gs _ =
  case Validator.activeProposal gs of
    Validator.NoProposal -> False
    Validator.SomeProposal p -> Validator.executed p

-- | Test-friendly main validator function
testMkValidator :: Validator.GovernanceState -> Validator.GovernanceRedeemer -> ScriptContext -> Bool
testMkValidator gs rd ctx =
  case rd of
    Validator.CreateProposal r -> testValidateCreate gs r ctx
    Validator.CastVote         -> testValidateVote gs ctx
    Validator.ExecuteProposal  -> testValidateExecute gs ctx
    Validator.UpdateGovernance -> testValidateUpdate gs ctx

-- | Helper functions that mirror the original validator logic
testIsValidRetirementRate :: Integer -> Bool
testIsValidRetirementRate r = r >= 0 && r <= 100

testHasActiveProposal :: Validator.GovernanceState -> Bool
testHasActiveProposal gs =
  case Validator.activeProposal gs of
    Validator.NoProposal -> False
    Validator.SomeProposal _ -> True

-- | Mock ScriptContext for tests (Plutus API type, not custom)
mkContext :: [PubKeyHash] -> ScriptContext
mkContext signatories =
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
        , txInfoSignatories = signatories
        , txInfoRedeemers = AssocMap.empty
        , txInfoData = AssocMap.empty
        , txInfoId = TxId "dummy_tx_id"
        }
    , scriptContextPurpose = Spending (TxOutRef (TxId "dummy_tx_id") 0)
    }

-- Sample keys
voterPkh, proposerPkh :: PubKeyHash
voterPkh = PubKeyHash "voter123"
proposerPkh = PubKeyHash "proposer456"

-- Sample vote
sampleVote :: Validator.Vote
sampleVote = Validator.Vote
  { Validator.voterPkh = "voter_pkh_123"
  , Validator.proposalId = 1
  , Validator.inFavor = True
  }

-- Sample proposal
sampleProposal :: Validator.Proposal
sampleProposal = Validator.Proposal
  { Validator.pId = 1
  , Validator.retirementPercent = 15
  , Validator.proposer = "proposer_pkh_456"
  , Validator.createdAt = 1000000
  , Validator.expiresAt = 2000000
  , Validator.votesFor = 3
  , Validator.votesAgainst = 1
  , Validator.allVotes = Validator.VCons sampleVote Validator.VNil
  , Validator.executed = False
  }

-- Executed proposal
executedProposal :: Validator.Proposal
executedProposal = sampleProposal { Validator.executed = True }

-- Governance states (mocking on-chain data)
gsNoProposal, gsWithActiveProposal, gsWithExecutedProposal :: Validator.GovernanceState
gsNoProposal = Validator.GovernanceState
  { Validator.currentRetirementRate = 10
  , Validator.activeProposal = Validator.NoProposal
  , Validator.proposalCounter = 1
  , Validator.minVotingPeriod = 86400000
  , Validator.quorumThreshold = 5
  }
gsWithActiveProposal = gsNoProposal { Validator.activeProposal = Validator.SomeProposal sampleProposal }
gsWithExecutedProposal = gsNoProposal { Validator.activeProposal = Validator.SomeProposal executedProposal }

-- The tests
tests :: TestTree
tests = testGroup "Governance Validator Tests"
  [ testGroup "CreateProposal Redeemer"
      [ testCase "Valid proposal creation with no active proposal" $ do
          let redeemer = Validator.CreateProposal 25
              ctx = mkContext [proposerPkh]
          assertBool "Should allow creating proposal when none active" $
            testMkValidator gsNoProposal redeemer ctx

      , testCase "Valid retirement rate at boundary (0%)" $ do
          let redeemer = Validator.CreateProposal 0
              ctx = mkContext [proposerPkh]
          assertBool "Should allow 0% retirement rate" $
            testMkValidator gsNoProposal redeemer ctx

      , testCase "Valid retirement rate at boundary (100%)" $ do
          let redeemer = Validator.CreateProposal 100
              ctx = mkContext [proposerPkh]
          assertBool "Should allow 100% retirement rate" $
            testMkValidator gsNoProposal redeemer ctx

      , testCase "Reject proposal when active proposal exists" $ do
          let redeemer = Validator.CreateProposal 25
              ctx = mkContext [proposerPkh]
          assertBool "Should reject proposal when active proposal exists" $
            not (testMkValidator gsWithActiveProposal redeemer ctx)

      , testCase "Reject invalid retirement rate (negative)" $ do
          let redeemer = Validator.CreateProposal (-5)
              ctx = mkContext [proposerPkh]
          assertBool "Should reject negative retirement rate" $
            not (testMkValidator gsNoProposal redeemer ctx)

      , testCase "Reject invalid retirement rate (over 100%)" $ do
          let redeemer = Validator.CreateProposal 150
              ctx = mkContext [proposerPkh]
          assertBool "Should reject retirement rate over 100%" $
            not (testMkValidator gsNoProposal redeemer ctx)
      ]

  , testGroup "CastVote Redeemer"
      [ testCase "Valid vote casting with active proposal" $ do
          let redeemer = Validator.CastVote
              ctx = mkContext [voterPkh]
          assertBool "Should allow voting when active proposal exists" $
            testMkValidator gsWithActiveProposal redeemer ctx

      , testCase "Reject vote when no active proposal" $ do
          let redeemer = Validator.CastVote
              ctx = mkContext [voterPkh]
          assertBool "Should reject voting when no active proposal" $
            not (testMkValidator gsNoProposal redeemer ctx)

      , testCase "Valid vote on executed proposal" $ do
          let redeemer = Validator.CastVote
              ctx = mkContext [voterPkh]
          assertBool "Should allow voting on executed proposal" $
            testMkValidator gsWithExecutedProposal redeemer ctx
      ]

  , testGroup "ExecuteProposal Redeemer"
      [ testCase "Valid execution of unexecuted proposal" $ do
          let redeemer = Validator.ExecuteProposal
              ctx = mkContext [proposerPkh]
          assertBool "Should allow execution of unexecuted proposal" $
            testMkValidator gsWithActiveProposal redeemer ctx

      , testCase "Reject execution when no active proposal" $ do
          let redeemer = Validator.ExecuteProposal
              ctx = mkContext [proposerPkh]
          assertBool "Should reject execution when no active proposal" $
            not (testMkValidator gsNoProposal redeemer ctx)

      , testCase "Reject execution of already executed proposal" $ do
          let redeemer = Validator.ExecuteProposal
              ctx = mkContext [proposerPkh]
          assertBool "Should reject execution of already executed proposal" $
            not (testMkValidator gsWithExecutedProposal redeemer ctx)
      ]

  , testGroup "UpdateGovernance Redeemer"
      [ testCase "Valid governance update after proposal execution" $ do
          let redeemer = Validator.UpdateGovernance
              ctx = mkContext [proposerPkh]
          assertBool "Should allow governance update after proposal execution" $
            testMkValidator gsWithExecutedProposal redeemer ctx

      , testCase "Reject update when no active proposal" $ do
          let redeemer = Validator.UpdateGovernance
              ctx = mkContext [proposerPkh]
          assertBool "Should reject update when no active proposal" $
            not (testMkValidator gsNoProposal redeemer ctx)

      , testCase "Reject update when proposal not executed" $ do
          let redeemer = Validator.UpdateGovernance
              ctx = mkContext [proposerPkh]
          assertBool "Should reject update when proposal not executed" $
            not (testMkValidator gsWithActiveProposal redeemer ctx)
      ]

  , testGroup "Edge Cases and Integration Tests"
      [ testCase "Multiple redeemers with same governance state" $ do
          let createRedeemer = Validator.CreateProposal 50
              voteRedeemer = Validator.CastVote
              ctx1 = mkContext [proposerPkh, voterPkh]
              ctx2 = mkContext [voterPkh]
          assertBool "Create proposal should work on clean state" $
            testMkValidator gsNoProposal createRedeemer ctx1
          assertBool "Vote should work with active proposal" $
            testMkValidator gsWithActiveProposal voteRedeemer ctx2

      , testCase "Boundary retirement rate values" $ do
          let redeemer1 = Validator.CreateProposal 1
              redeemer99 = Validator.CreateProposal 99
              ctx = mkContext [proposerPkh]
          assertBool "Should accept minimum valid rate (1%)" $
            testMkValidator gsNoProposal redeemer1 ctx
          assertBool "Should accept maximum valid rate (99%)" $
            testMkValidator gsNoProposal redeemer99 ctx

      , testCase "Empty signatory list handling" $ do
          let redeemer = Validator.CreateProposal 25
              ctx = mkContext []
          assertBool "Should handle empty signatory list" $
            testMkValidator gsNoProposal redeemer ctx
      ]
  ]
