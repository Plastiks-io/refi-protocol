{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unused-imports -Wno-unused-local-binds -Wno-name-shadowing -Wno-dodgy-imports #-}

module Governance.Validator (Vote(..),
                VoteList(..),
                Proposal(..),
                MaybeProposal(..),
                GovernanceState(..),
                GovernanceRedeemer(..),
                mkValidator,
                validator
                ) where

import           GHC.Generics (Generic)
import qualified PlutusTx.Prelude as P
import           PlutusTx
import           PlutusTx.Prelude hiding (error)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts
import qualified Prelude as Haskell
import           Prelude (Show)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Codec.Serialise (serialise)

-- VOTE
data Vote = Vote
  { voterPkh   :: BuiltinByteString
  , proposalId :: Integer
  , inFavor    :: Bool
  }
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''Vote [('Vote,0)]

-- VOTELIST
data VoteList
  = VNil
  | VCons Vote VoteList
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''VoteList [('VNil,0),('VCons,1)]

-- PROPOSAL
data Proposal = Proposal
  { pId               :: Integer
  , retirementPercent :: Integer
  , proposer          :: BuiltinByteString
  , createdAt         :: POSIXTime
  , expiresAt         :: POSIXTime
  , votesFor          :: Integer
  , votesAgainst      :: Integer
  , allVotes          :: VoteList
  , executed          :: Bool
  }
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''Proposal [('Proposal,0)]

-- MAYBE PROPOSAL
data MaybeProposal
  = NoProposal
  | SomeProposal Proposal
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''MaybeProposal [('NoProposal,0),('SomeProposal,1)]

-- GOVERNANCE STATE
data GovernanceState = GovernanceState
  { currentRetirementRate :: Integer
  , activeProposal        :: MaybeProposal
  , proposalCounter       :: Integer
  , minVotingPeriod       :: POSIXTime
  , quorumThreshold       :: Integer
  }
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''GovernanceState [('GovernanceState,0)]

-- REDEEMER
data GovernanceRedeemer
  = CreateProposal Integer
  | CastVote
  | ExecuteProposal
  | UpdateGovernance
  deriving stock (Haskell.Show, Generic)
PlutusTx.makeIsDataIndexed ''GovernanceRedeemer
  [ ('CreateProposal,0)
  , ('CastVote,1)
  , ('ExecuteProposal,2)
  , ('UpdateGovernance,3)
  ]

-- TRACE HELPERS
{-# INLINABLE governanceTraceError #-}
governanceTraceError :: BuiltinString -> a
governanceTraceError = P.traceError

{-# INLINABLE governanceTraceIfFalse #-}
governanceTraceIfFalse :: BuiltinString -> Bool -> Bool
governanceTraceIfFalse msg cond =
  if cond then True else P.traceError msg

-- UTILITY CHECKS
{-# INLINABLE isValidRetirementRate #-}
isValidRetirementRate :: Integer -> Bool
isValidRetirementRate r = r >= 0 && r <= 100

{-# INLINABLE hasActiveProposal #-}
hasActiveProposal :: GovernanceState -> Bool
hasActiveProposal gs =
  case activeProposal gs of
    NoProposal     -> False
    SomeProposal _ -> True

-- Allow fallback instead of failing
{-# INLINABLE getCurrentTime #-}
getCurrentTime :: ScriptContext -> POSIXTime
getCurrentTime ctx =
  case ivFrom (txInfoValidRange $ scriptContextTxInfo ctx) of
    LowerBound (Finite t) _ -> t
    _                       -> 0  -- default if unbounded

-- VALIDATOR LOGIC
{-# INLINABLE validateCreate #-}
validateCreate :: GovernanceState -> Integer -> ScriptContext -> Bool
validateCreate gs rate _ =
  governanceTraceIfFalse "existing-proposal" (not $ hasActiveProposal gs) &&
  governanceTraceIfFalse "invalid-rate" (isValidRetirementRate rate)

{-# INLINABLE validateVote #-}
validateVote :: GovernanceState -> ScriptContext -> Bool
validateVote gs _ =
  case activeProposal gs of
    NoProposal     -> governanceTraceError "no-active-proposal"
    SomeProposal _ -> True

{-# INLINABLE validateExecute #-}
validateExecute :: GovernanceState -> ScriptContext -> Bool
validateExecute gs _ =
  case activeProposal gs of
    NoProposal     -> governanceTraceError "no-active-proposal-found"
    SomeProposal p ->
      governanceTraceIfFalse "already-executed" (not $ executed p)

{-# INLINABLE validateUpdate #-}
validateUpdate :: GovernanceState -> ScriptContext -> Bool
validateUpdate gs _ =
  case activeProposal gs of
    NoProposal     -> governanceTraceError "no-active-proposal"
    SomeProposal p -> governanceTraceIfFalse "not-executed" (executed p)

{-# INLINABLE mkValidator #-}
mkValidator :: GovernanceState -> GovernanceRedeemer -> ScriptContext -> Bool
mkValidator gs rd ctx =
  case rd of
    CreateProposal r -> validateCreate gs r ctx
    CastVote         -> validateVote gs ctx
    ExecuteProposal  -> validateExecute gs ctx
    UpdateGovernance -> validateUpdate gs ctx

{-# INLINABLE mkUntyped #-}
mkUntyped :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkUntyped d r c =
  let gs  = unsafeFromBuiltinData @GovernanceState d
      rd' = unsafeFromBuiltinData @GovernanceRedeemer r
      ctx = unsafeFromBuiltinData @ScriptContext c
  in if mkValidator gs rd' ctx then () else P.traceError "validation-failed"

-- COMPILE & EXPORT
validator :: Validator
validator = mkValidatorScript $$(compile [|| mkUntyped ||])

script :: Script
script = unValidatorScript validator

scriptShortBs :: SBS.ShortByteString
scriptShortBs = SBS.toShort . LBS.toStrict $ serialise script
