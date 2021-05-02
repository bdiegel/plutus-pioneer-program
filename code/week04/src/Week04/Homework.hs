{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import Control.Monad.Freer.Extras as Extras
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import GHC.Generics               (Generic)
import Ledger
import Ledger.Ada                 as Ada
import Ledger.Constraints         as Constraints
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = BlockchainActions .\/ Endpoint "pay" PayParams

payContract' :: Contract () PaySchema Text ()
payContract' = do
    -- this will block until the endpoint is called
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    -- we recursively call ourself in order to allow any number of payments
    payContract'

payContract :: Contract () PaySchema Void ()
payContract = Contract.handleError 
    (\err -> Contract.logError  $ "caught err: " ++ unpack err) 
    payContract'

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
--payTrace x y = undefined -- IMPLEMENT ME!
payTrace x y = do
    h <- activateContractWallet (Wallet 1) payContract
    -- h2 <- activateContractWallet (Wallet 1) payContract
    callEndpoint @"pay" h $ PayParams 
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace = x
        }
    s1 <- waitUntilSlot 1
    Extras.logInfo $ "reached slot " ++ show s1
    callEndpoint @"pay" h $ PayParams 
        { ppRecipient = pubKeyHash $ walletPubKey $ Wallet 2
        , ppLovelace = y
        }
    s2 <- waitUntilSlot 1
    Extras.logInfo $ "reached slot " ++ show s2



payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

-- this test will generate a WalletError for InsufficientFunds
-- we must add handling to payContract to handle error and prevent contract from crashing
payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000000000 2000000

{-
   // example: fatal error before handling was added
   Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"WalletError (InsufficientFunds \\\"Total: Value (Map [(,Map [(\\\\\\\"\\\\\\\",100000000)])]) expected: Value (Map [(,Map [(\\\\\\\"\\\\\\\",1000000010)])])\\\")\""

   // example: logged error caught by handler
   Slot 00001: *** CONTRACT STOPPED WITH ERROR: "\"WalletError (InsufficientFunds \\\"Total: Value (Map [(,Map [(\\\\\\\"\\\\\\\",100000000)])]) expected: Value (Map [(,Map [(\\\\\\\"\\\\\\\",1000000010)])])\\\")\""
   Contract instance stopped (no errors)
-}