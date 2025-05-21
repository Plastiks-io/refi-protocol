{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}

module Refi.Validator (  PlastiksDatum(..),
                        PlastiksRedeemer(..),
                        validate,
                        wrapped,
                        validator) where
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Prelude (Show)

-------------------------------------------------
-- Datum and Redeemer
-------------------------------------------------

data PlastiksDatum = PlastiksDatum
    { preId :: BuiltinByteString
    , roadmapId :: BuiltinByteString
    , roadmapName :: BuiltinByteString       
    , roadmapDescription :: BuiltinByteString
    , progress :: Integer
    , adminPkh :: PubKeyHash
    , prePkh :: PubKeyHash
    , preSkh :: PubKeyHash 
    , totalPlasticCredits :: Integer
    , soldPlasticCredits :: Integer
    , totalPlasticTokens :: Integer
    , sentPlasticTokens :: Integer
    , totalPlastic::Integer
    , recoverPlastic::Integer
    , createdAt:: BuiltinByteString
    }  deriving stock Show

PlutusTx.unstableMakeIsData ''PlastiksDatum

data PlastiksRedeemer = UpdateProgress Integer | Release | Archived
    deriving stock Show

PlutusTx.unstableMakeIsData ''PlastiksRedeemer

-------------------------------------------------
-- Validator Logic
-------------------------------------------------

{-# INLINABLE validate #-}
validate :: PlastiksDatum -> PlastiksRedeemer -> ScriptContext -> Bool
validate datum redeemer ctx =
    case redeemer of
        UpdateProgress newProgress ->
            traceIfFalse "Admin not signed" (txSignedBy info (adminPkh datum)) &&
            traceIfFalse "Invalid progress update" (newProgress > progress datum && newProgress <= 10000)  -- Scaled to 100.00%

        Release ->
            traceIfFalse "Admin not signed" (txSignedBy info (adminPkh datum)) &&
            traceIfFalse "Progress not complete" (progress datum == 10000)  -- Must reach 100.00%

        Archived ->
            traceIfFalse "Admin not signed" (txSignedBy info (adminPkh datum))
            
    where
        info = scriptContextTxInfo ctx

-------------------------------------------------
-- Wrap Validator
-------------------------------------------------

{-# INLINABLE wrapped #-}
wrapped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
wrapped d r c =
    check $ validate
        (unsafeFromBuiltinData d)
        (unsafeFromBuiltinData r)
        (unsafeFromBuiltinData c)

validator :: Validator
validator = mkValidatorScript $$(compile [|| wrapped ||])
