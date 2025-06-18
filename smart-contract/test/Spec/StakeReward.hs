{-# LANGUAGE OverloadedStrings #-}
 
module Spec.StakeReward (tests) where
 
import qualified Data.ByteString.Char8 as BS8
import qualified Lend.StakeReward as SR
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Api (PubKeyHash (..))
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx.AssocMap as AssocMap
import qualified PlutusTx.Builtins as Builtins
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Prelude
 
-- Admin and lender identities (mock values)
admin1 :: PubKeyHash
admin1 = PubKeyHash (Builtins.toBuiltin (BS8.pack "admin"))
 
lender1 :: PubKeyHash
lender1 = PubKeyHash (Builtins.toBuiltin (BS8.pack "lender"))
 
-- Sample Datum
initialDatum :: SR.LenderDatum
initialDatum =
  SR.LenderDatum
    { SR.adminsPkh = [admin1],
      SR.totalPT = 10000,
      SR.totalReward = 5000,
      SR.lenders = [(lender1, (5000, 2500))]
    }
 
-- Modified data
updatedDatumPT = initialDatum {SR.totalPT = 12000}
 
updatedDatumReward = initialDatum {SR.totalReward = 6000}
 
-- an unauthorized user (a lender pretending to be an admin) tries to perform admin-only actions
invalidDatum = initialDatum {SR.adminsPkh = [lender1]}
 
-- Wrap datum into OutputDatum
toOutputDatum :: SR.LenderDatum -> OutputDatum
toOutputDatum = OutputDatum . Datum . toBuiltinData
 
-- Construct ScriptContext
mkCtx :: Bool -> [TxOut] -> ScriptContext
mkCtx signed continuingOutputs =
  ScriptContext
    { scriptContextTxInfo =
        TxInfo
          { txInfoInputs = [],
            txInfoReferenceInputs = [],
            txInfoOutputs = continuingOutputs,
            txInfoFee = mempty,
            txInfoMint = mempty,
            txInfoDCert = [],
            txInfoWdrl = AssocMap.empty,
            txInfoValidRange = always,
            txInfoSignatories = if signed then [admin1] else [],
            txInfoRedeemers = AssocMap.empty,
            txInfoData = AssocMap.empty,
            txInfoId = TxId "dummy"
          },
      scriptContextPurpose = Spending (TxOutRef (TxId "dummy") 0)
    }
 
-- Create output with datum
mkContinuingOutput :: SR.LenderDatum -> TxOut
mkContinuingOutput dat =
  TxOut
    { txOutAddress = Address (ScriptCredential "dummy") Nothing,
      txOutValue = mempty,
      txOutDatum = toOutputDatum dat,
      txOutReferenceScript = Nothing
    }
 
--  Mock validator simulating business logic off-chain for test purposes
mockValidator :: SR.LenderDatum -> SR.LenderAction -> ScriptContext -> Bool
mockValidator oldDatum action ctx =
  case action of
    SR.Deposit ->
      let newDatum = extractDatumFromCtx ctx
       in SR.adminsPkh newDatum == SR.adminsPkh oldDatum
    SR.Withdraw amt ->
      amt > 0 && amt <= getLenderPT lender1 oldDatum
    SR.Redeem ->
      let outs = txInfoOutputs (scriptContextTxInfo ctx)
       in case outs of
            [] -> True
            [TxOut {txOutDatum = OutputDatum (Datum d)}] ->
              case fromBuiltinData d of
                Just newDatum -> SR.totalReward newDatum <= SR.totalReward oldDatum
                _ -> False
            _ -> False
    SR.FundPlastikToEscrow _ ->
      admin1 `elem` txInfoSignatories (scriptContextTxInfo ctx)
    SR.FundUSDM _ ->
      admin1 `elem` txInfoSignatories (scriptContextTxInfo ctx)
  where
    extractDatumFromCtx :: ScriptContext -> SR.LenderDatum
    extractDatumFromCtx sc =
      let outs = txInfoOutputs (scriptContextTxInfo sc)
       in case outs of
            [TxOut {txOutDatum = OutputDatum (Datum d)}] ->
              case fromBuiltinData d of
                Just dt -> dt
                _ -> error "Invalid datum in output"
            _ -> oldDatum
 
    getLenderPT :: PubKeyHash -> SR.LenderDatum -> Integer
    getLenderPT pkh datum =
      case lookup pkh (SR.lenders datum) of
        Just (pt, _) -> pt
        Nothing -> 0
 
--  Tests
tests :: TestTree
tests =
  testGroup
    "Plastiks StakeReward Validator Tests"
    [ testGroup
        "Deposit"
        [ testCase "Valid deposit with unchanged admins" $
            assertBool "Should pass" $
              mockValidator initialDatum SR.Deposit (mkCtx True [mkContinuingOutput updatedDatumPT]),
          testCase "Invalid deposit with admin changed" $
            assertBool "Should fail" $
              not (mockValidator initialDatum SR.Deposit (mkCtx True [mkContinuingOutput invalidDatum]))
        ],
      testGroup
        "Withdraw"
        [ testCase "Valid withdraw" $
            assertBool "Should pass" $
              mockValidator initialDatum (SR.Withdraw 1000) (mkCtx True [mkContinuingOutput updatedDatumPT]),
          testCase "Invalid withdraw amount (0)" $
            assertBool "Should fail" $
              not (mockValidator initialDatum (SR.Withdraw 0) (mkCtx True [mkContinuingOutput updatedDatumPT])),
          testCase "Invalid withdraw amount (more than lended)" $
            assertBool "Should fail" $
              not (mockValidator initialDatum (SR.Withdraw 6000) (mkCtx True [mkContinuingOutput updatedDatumPT]))
        ],
      testGroup
        "Redeem"
        [ testCase "Valid redeem with no continuing output" $
            assertBool "Should pass full redeem" $
              mockValidator initialDatum SR.Redeem (mkCtx True []),
          testCase "Valid redeem with reduced reward" $
            let newDatum = initialDatum {SR.totalReward = 4000}
             in assertBool "Should pass partial redeem" $
                  mockValidator initialDatum SR.Redeem (mkCtx True [mkContinuingOutput newDatum]),
          testCase "Invalid redeem with increased reward" $
            assertBool "Should fail" $
              not (mockValidator initialDatum SR.Redeem (mkCtx True [mkContinuingOutput updatedDatumReward]))
        ],
      testGroup
        "FundPlastikToEscrow"
        [ testCase "Valid admin withdraw" $
            assertBool "Should pass" $
              mockValidator initialDatum (SR.FundPlastikToEscrow 1000) (mkCtx True [mkContinuingOutput initialDatum]),
          testCase "Invalid without admin signature" $
            assertBool "Should fail" $
              not (mockValidator initialDatum (SR.FundPlastikToEscrow 1000) (mkCtx False [mkContinuingOutput initialDatum]))
        ],
      testGroup
        "FundUSDM"
        [ testCase "Valid USDM fund with increased reward" $
            assertBool "Should pass" $
              mockValidator initialDatum (SR.FundUSDM 1000) (mkCtx True [mkContinuingOutput updatedDatumReward]),
          testCase "Invalid USDM fund without admin" $
            assertBool "Should fail" $
              not (mockValidator initialDatum (SR.FundUSDM 1000) (mkCtx False [mkContinuingOutput updatedDatumReward]))
        ]
    ]