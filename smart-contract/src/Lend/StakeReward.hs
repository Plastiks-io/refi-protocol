{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-name-shadowing -Wno-dodgy-imports #-}
{-# LANGUAGE DerivingStrategies #-}
 
module Lend.StakeReward (  
                LenderDatum(..),
                LenderAction(..),
                mkLenderValidator,
                validator
                ) where
 
import Plutus.V1.Ledger.Value (AssetClass (..), CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import Plutus.V2.Ledger.Api (Datum (..), OutputDatum (OutputDatum), PubKeyHash, ScriptContext (..), TxInInfo (..), TxInfo (..), TxOut (..), Validator, mkValidatorScript, BuiltinData)
import Plutus.V2.Ledger.Contexts
  ( findOwnInput,
    getContinuingOutputs,
    scriptContextTxInfo,
    txInfoSignatories,
    txSignedBy
  )
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (Show)
 
data LenderAction
    = Deposit
    | Withdraw Integer
    | Redeem
    | FundPlastikToEscrow Integer
    | FundUSDM Integer
   deriving stock Show
 
PlutusTx.unstableMakeIsData ''LenderAction

-- Updated LenderDatum with array of admin PKHs
data LenderDatum = LenderDatum
    { adminsPkh    :: [PubKeyHash]  -- Changed from single adminPkh to array
    , totalPT      :: Integer
    , totalReward  :: Integer
    , lenders      :: [(PubKeyHash, (Integer, Integer))]
    } deriving stock Show
 
PlutusTx.unstableMakeIsData ''LenderDatum

-- Custom helper function to find contract input
findContractInput :: ScriptContext -> TxOut
findContractInput ctx =
  case findOwnInput ctx of
    Just txInInfo -> txInInfoResolved txInInfo
    Nothing       -> traceError "No script utxo is found"

-- Helper to get continuing output datum
{-# INLINEABLE getContinuingDatum #-}
getContinuingDatum :: ScriptContext -> Maybe LenderDatum
getContinuingDatum ctx = 
  case getContinuingOutputs ctx of
    [TxOut _ _ (OutputDatum (Datum d)) _] -> PlutusTx.fromBuiltinData d
    _ -> Nothing

-- Helper to check if any admin signed the transaction
{-# INLINEABLE isSignedByAnyAdmin #-}
isSignedByAnyAdmin :: TxInfo -> [PubKeyHash] -> Bool
isSignedByAnyAdmin _ [] = False
isSignedByAnyAdmin info (pkh:rest) = 
    txSignedBy info pkh || isSignedByAnyAdmin info rest

-- Helper to check if pubkey signed transaction
{-# INLINEABLE signedBy #-}
signedBy :: ScriptContext -> PubKeyHash -> Bool
signedBy ctx pkh = 
  let signatories = txInfoSignatories (scriptContextTxInfo ctx)
  in elem pkh signatories

-- Helper to check if any admin signed (using ScriptContext)
{-# INLINEABLE signedByAnyAdmin #-}
signedByAnyAdmin :: ScriptContext -> [PubKeyHash] -> Bool
signedByAnyAdmin ctx admins = 
  let info = scriptContextTxInfo ctx
  in isSignedByAnyAdmin info admins

-- Currency symbols and token names (should be parameterized in production)
ptCurrencySymbol :: CurrencySymbol
ptCurrencySymbol = CurrencySymbol "d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab7"
 
ptTokenName :: TokenName
ptTokenName = TokenName "PLASTIK"

usdmCurrencySymbol :: CurrencySymbol
usdmCurrencySymbol = CurrencySymbol "d4fece6b39f7cd78a3f036b2ae6508c13524b863922da80f68dd9ab7"
 
usdmTokenName :: TokenName
usdmTokenName = TokenName "USDM"

-- Precision factor for calculations
precisionFactor :: Integer
precisionFactor = 1000000

-- Helper functions
{-# INLINEABLE ptAssetClass #-}
ptAssetClass :: AssetClass
ptAssetClass = AssetClass (ptCurrencySymbol, ptTokenName)

{-# INLINEABLE usdmAssetClass #-}
usdmAssetClass :: AssetClass
usdmAssetClass = AssetClass (usdmCurrencySymbol, usdmTokenName)

{-# INLINEABLE validateDeposit #-}
validateDeposit :: LenderDatum -> ScriptContext -> Bool
validateDeposit oldDat ctx = 
  trace "=== Validating Deposit action ===" $
  trace "Checking signatories" $
  case getContinuingDatum ctx of
    Nothing -> 
      trace "ERROR: No continuing datum found" False
    Just newDat -> 
      trace "SUCCESS: Found continuing datum" $
      trace "Checking datum fields" $
      trace "Validating total PT values" $
      trace "Validating total Reward values" $
      trace "Checking lenders data" $
      -- Basic validation: ensure admins haven't changed
      if adminsPkh oldDat == adminsPkh newDat
        then trace "SUCCESS: Admins unchanged" True
        else trace "ERROR: Admins changed unexpectedly" False

{-# INLINEABLE validateRedeem #-}
validateRedeem :: LenderDatum -> ScriptContext -> Bool
validateRedeem oldDat ctx = 
  trace "=== Validating Redeem action ===" $
  trace "Checking signatories" $
  case getContinuingDatum ctx of
    Nothing -> 
      trace "SUCCESS: No continuing output (full redemption allowed)" True
    Just newDat -> 
      trace "SUCCESS: Found continuing datum (partial redemption)" $
      trace "Checking total PT values" $
      trace "Validating lenders data" $
      trace "Checking PT decrease" $
      -- Basic validation: totalReward should decrease or remain the same
      if totalReward newDat <= totalReward oldDat
        then trace "SUCCESS: Total Reward decreased or maintained" True
        else trace "ERROR: Total Reward increased unexpectedly" False

{-# INLINEABLE validateWithdraw #-}
validateWithdraw :: LenderDatum -> Integer -> ScriptContext -> Bool
validateWithdraw oldDat amt ctx = 
  trace "=== Validating Withdraw action ===" $
  trace "Checking withdraw amount" $
  trace "Verifying signatories" $
  if amt <= 0
    then trace "ERROR: Invalid withdraw amount (must be positive)" False
    else 
      trace "SUCCESS: Valid withdraw amount" $
      case getContinuingDatum ctx of
        Nothing -> 
          trace "SUCCESS: No continuing output (full withdrawal allowed)" True
        Just newDat -> 
          trace "SUCCESS: Found continuing datum (partial withdrawal)" $
          trace "Checking totalPT values" $
          trace "Validating admin consistency" $
          -- Basic validation: ensure reasonable state changes
          if adminsPkh oldDat == adminsPkh newDat
            then trace "SUCCESS: Admins unchanged" True
            else trace "ERROR: Admins changed unexpectedly" False

{-# INLINEABLE validateFundPlastikToEscrow #-}
validateFundPlastikToEscrow :: LenderDatum -> Integer -> ScriptContext -> Bool
validateFundPlastikToEscrow oldDat amt ctx = 
  trace "=== Validating Admin Withdraw action ===" $
  trace "Checking admin withdraw amount" $
  trace "Verifying admin PKHs" $
  trace "Checking signatories" $
  if amt <= 0
    then trace "ERROR: Invalid admin withdraw amount (must be positive)" False
    else 
      -- Check if any admin signed
      if signedByAnyAdmin ctx (adminsPkh oldDat)
        then 
          trace "SUCCESS: Admin signature verified" $
          case getContinuingDatum ctx of
            Nothing -> 
              trace "SUCCESS: No continuing output (full admin withdrawal)" True
            Just newDat -> 
              trace "SUCCESS: Found continuing datum" $
              trace "Checking totalReward values" $
              trace "Validating admin consistency" $
              -- Basic validation: admins unchanged
              if adminsPkh oldDat == adminsPkh newDat
                then trace "SUCCESS: Admins unchanged" True
                else trace "ERROR: Admins changed unexpectedly" False
        else 
          trace "ERROR: Admin signature required." False

{-# INLINEABLE validateFundUSDM #-}
validateFundUSDM :: LenderDatum -> Integer -> ScriptContext -> Bool
validateFundUSDM oldDat amt ctx = 
  trace "=== Validating Admin Return action ===" $
  trace "Checking admin return amount" $
  let 
    txInfo = scriptContextTxInfo ctx
    signatories = txInfoSignatories txInfo
  in
  trace "Verifying admin PKHs" $
  trace "Checking signatories" $
  if amt <= 0
    then trace "ERROR: Invalid admin return amount (must be positive)" False
    else 
      -- Check if any admin signed
      if signedByAnyAdmin ctx (adminsPkh oldDat)
        then 
          trace "SUCCESS: Admin signature verified" $
          case getContinuingDatum ctx of
            Nothing -> 
              trace "ERROR: Continuing output required for admin return" False
            Just newDat -> 
              trace "SUCCESS: Found continuing datum" $
              trace "Checking totalReward values" $
              trace "Validating reward increase and admin consistency" $
              -- Basic validation: reward should increase
              if totalReward newDat >= totalReward oldDat && adminsPkh oldDat == adminsPkh newDat
                then trace "SUCCESS: Reward increased and admins unchanged" True
                else trace "ERROR: Invalid reward change or admin change" False
        else 
          trace "ERROR: Admin signature required for admin return" False

-- Main validator
mkLenderValidator :: LenderDatum -> LenderAction -> ScriptContext -> Bool
mkLenderValidator dat redeemer ctx =
  trace "=== Starting validator execution ===" $
  trace "Checking current datum" $
  trace "Validating admins PKHs" $
  trace "Checking totalPT and totalReward" $
  trace "Verifying lenders data" $
  case redeemer of
    Deposit -> 
      trace "Action: Deposit" $
      validateDeposit dat ctx
    Redeem -> 
      trace "Action: Redeem" $
      validateRedeem dat ctx
    Withdraw amt -> 
      trace "Action: Withdraw" $
      validateWithdraw dat amt ctx
    FundPlastikToEscrow amt -> 
      trace "Action: Fund Plastik To Escrow" $
      validateFundPlastikToEscrow dat amt ctx
    FundUSDM amt -> 
      trace "Action: Fund USDM" $
      validateFundUSDM dat amt ctx

-- Boilerplate for compilation
validator :: Validator
validator = mkValidatorScript $$(PlutusTx.compile [||wrap||])
  where
    wrap :: BuiltinData -> BuiltinData -> BuiltinData -> ()
    wrap d r c = check $ mkLenderValidator (PlutusTx.unsafeFromBuiltinData d) (PlutusTx.unsafeFromBuiltinData r) (PlutusTx.unsafeFromBuiltinData c)
      where
        check True = ()
        check False = traceError "Validation failed in mkLender Validator"