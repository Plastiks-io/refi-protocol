{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
 
module Lend.Compiler (writeValidatorScript) where
 
import Cardano.Api
  ( FileError,
    PlutusScript,
    PlutusScriptV2,
    writeFileTextEnvelope,
  )
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import PlutusTx.Prelude (Either, Maybe (Nothing), (.))
import Lend.StakeReward as Validator (validator)
import Prelude (FilePath, IO)
 
writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript
 
writeValidatorScript :: IO (Either (FileError ()) ())
writeValidatorScript = writeValidator "output/reward.json" Validator.validator
 