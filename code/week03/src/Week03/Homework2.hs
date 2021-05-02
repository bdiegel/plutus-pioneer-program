{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week03.Homework2 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON, Value (Bool))
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      hiding (when)
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (Semigroup (..))
import           Text.Printf          (printf)

{-
   description: This is a Paramaterized version of Vesting contract defined by Vesting.hs.
   
   Note that this is not the same as the homework 1 contract - where the the goal was to
   add a second beneficiary. This is the original signal beneficiary contract, so we only
   need to verify that the beneficiary has signed and that the deadline has been reached.
-}

{-# INLINABLE mkValidator #-}
mkValidator :: PubKeyHash -> Slot -> () -> ScriptContext -> Bool
mkValidator sig slot _ ctx =
    traceIfFalse "not signed by beneficiary" (not (txSignedBy info sig)) &&
    traceIfFalse "deadline not reached"      checkDeadline
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkDeadline :: Bool
    checkDeadline = from slot `contains` txInfoValidRange info


data Vesting
instance Scripts.ScriptType Vesting where
    type instance DatumType Vesting = Slot
    type instance RedeemerType Vesting = ()

{- 
   This is almost the same as inst defined by example Parameterized.hs 
   We need to match the type of mkValidator, which takes PubKeyHash (p) and Slot
   Note that RedeemerType is ().

   This is binding the script contract to the PubKeyHash specified by the give 
   transaction in the off-chain code.
-}
inst :: PubKeyHash -> Scripts.ScriptInstance Vesting
inst p = Scripts.validator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @Slot @()

{-
   Note here the type change - the validator now expects a PubKeyHash

   The following expression is equivalent to the definition below:
       validator p = Scripts.validatorScript $ inst p

   Since the validator and inst functions both take the same argument, we can eta-reduce
   the expression and remove the parameter altogether. Also note that we used composition (.) 
   rather than function application ($) in the reduced expression.
-}
validator :: PubKeyHash -> Validator
validator = Scripts.validatorScript . inst


{-
   Note here the type change from the Vesting.hs example. 
   
   The expression below is equivalent to:
      scrAddress p = scriptAddress $ validator p
-}
scrAddress :: PubKeyHash -> Ledger.Address
scrAddress = scriptAddress . validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PubKeyHash
    , gpDeadline    :: !Slot
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
    BlockchainActions
        .\/ Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: (HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
give gp = do
    let p  = gpBeneficiary gp
        d  = gpDeadline gp
        tx = mustPayToTheScript d $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints (inst p) tx
    void $ awaitTxConfirmed $ txId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. (HasBlockchainActions s, AsContractError e) => Contract w s e ()
grab = do
    now   <- currentSlot
    pkh   <- pubKeyHash <$> ownPubKey
    utxos <- Map.filter (isSuitable now) <$> utxoAt (scrAddress pkh)
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos        <>
                          Constraints.otherScript (validator pkh)
                tx :: TxConstraints Void Void
                tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toData () | oref <- orefs] <>
                          mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ txId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: Slot -> TxOutTx -> Bool
    isSuitable now o = case txOutDatumHash $ txOutTxOut o of
        Nothing -> False
        Just h  -> case Map.lookup h $ txData $ txOutTxTx o of
            Nothing        -> False
            Just (Datum e) -> case PlutusTx.fromData e of
                Nothing -> False
                Just d  -> d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" >>= give
    grab' = endpoint @"grab" >>  grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
