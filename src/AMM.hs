{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AMM where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1
  ( CurrencySymbol (..),
    TokenName (..),
    adaSymbol,
    adaToken,
    flattenValue,
    symbols,
    valueOf,
  )
import PlutusLedgerApi.V2
  ( Credential (..),
    PubKeyHash,
    ScriptContext (..),
    addressCredential,
    scriptContextTxInfo,
    txInfoOutputs,
    txOutAddress,
    txOutValue,
  )
import PlutusLedgerApi.V2 as PlutusV2
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins as Builtins
import PlutusTx.List as List
import PlutusTx.Prelude as P
import qualified PlutusTx.Show as PlutusTx

type LPPKH = PubKeyHash

type Amount = Integer

type LPTokenNumber = Integer

type AdaReserve = Integer

type TokenReserve = Integer

type LPTokenCurrencySymbol = CurrencySymbol
type TokenSymbol = CurrencySymbol
type LPTokenName = TokenName

data PoolParams = PoolParams {
    adaR:: AdaReserve,
    tokR:: TokenReserve,
    tokSymbol::TokenSymbol,
    lpTokNumb:: LPTokenNumber,
    lpPKH::  PubKeyHash,
    lpCurrSymb:: LPTokenCurrencySymbol,
    lpTokName:: LPTokenName
   } deriving stock (Generic)
     deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed ''PoolParams [('PoolParams,0)]
--Redeemer have 3 parts:
-- 1. AddLiquitdity
-- 2. Swap a token to ada or swap a ada to token
-- 3. Remove the liquidity
data AMMRedeemer
  = AddLiquidity Amount Amount PoolParams
  | Swap Amount Amount AdaReserve TokenReserve LPTokenCurrencySymbol
  | RemoveLiquidity Amount LPPKH
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

PlutusTx.makeIsDataSchemaIndexed
  ''AMMRedeemer
  [ ('AddLiquidity, 0),
    ('Swap, 1),
    ('RemoveLiquidity, 2)
  ]

{-# INLINEABLE ammValidator #-}
ammValidator :: AMMRedeemer -> ScriptContext -> Bool
ammValidator r ctx =
  case r of
    AddLiquidity adaAmount tokenAmount poolParams ->
      P.traceIfFalse "AddLiquidty fails" (addLiquidity adaAmount tokenAmount poolParams ctx)
    Swap amount minAmount adaR tokR tokSymbol ->
      P.traceIfFalse "Swap fails" (swap amount minAmount adaR tokR tokSymbol ctx)
    --   P.traceIfFalse "Swap fails" (swap amount tokSymbol lpPKH ctx)
-- Swap tA tB amount owner ->
--   P.traceIfFalse "Swap fails" (swap tA tB amount owner ctx)
-- RemoveLiquidity tA tB amount owner ->
--   P.traceIfFalse "Removal fails" (removeLiquidity tA tB amount owner ctx)


-- ajout de liquidité
{-# INLINEABLE addLiquidity #-}
addLiquidity :: Amount -> Amount -> PoolParams -> ScriptContext -> Bool
addLiquidity adaAmount tokenAmount poolParams ctx =
  if adaAmount <= 0 || tokenAmount <= 0
    then False
    else
      -- au cas ou il n'ya pas de reserve du token ajouté
      if (tokR poolParams) == 0
        then
          let tokensForLp = (adaR poolParams) P.+ adaAmount
           in finalizeAdding adaAmount tokenAmount tokensForLp poolParams ctx
        else
          let tokensForLp = P.divide ((lpTokNumb poolParams) P.* adaAmount) (adaR poolParams)
           in finalizeAdding adaAmount tokenAmount tokensForLp poolParams ctx

{-# INLINEABLE finalizeAdding #-}
finalizeAdding :: Amount -> Amount -> LPTokenNumber -> PoolParams -> ScriptContext -> Bool
finalizeAdding adaAmount tokenAmount tokensForLp poolParams ctx =
  List.and [
      goodReserves (adaR poolParams) (tokR poolParams) ctx,
      lpInput (tokSymbol poolParams) ctx,
      scriptOuput (adaR poolParams) (tokR poolParams) (tokSymbol poolParams) ctx,
      lpOutput poolParams ctx
   ]
  where
    -- Je verifie les reserves en m'assurant que les reserves précisées correspondent
    -- bien a ce qui avait été hachés et sotckées dans le datumHash du pool
    goodReserves :: AdaReserve -> TokenReserve -> ScriptContext -> Bool
    goodReserves adaR tokR ctx =
      let datumHash = getHash adaR tokR
          inputs = txInfoInputs $ scriptContextTxInfo ctx
       in List.any
            ( \inp ->
                case addressCredential $ txOutAddress $ txInInfoResolved inp of
                  ScriptCredential _ -> case txOutDatum (txInInfoResolved inp) of
                    OutputDatumHash (DatumHash hash) -> datumHash == hash
                    _ -> False
                  _ -> False
            )
            inputs

    -- inputs
    lpInput :: LPTokenCurrencySymbol -> ScriptContext -> Bool
    lpInput tokSymb ctx =
      let inputs = txInfoInputs $ scriptContextTxInfo ctx
          -- verifier que le lp a le montant d'Ada qu'il fournit
          goodAdaInput =
            List.any
              ( \inp ->
                  List.any
                    (\(cs, _, am) -> (cs P.== adaSymbol) P.&& (am P.>= adaAmount))
                    (flattenValue $ txOutValue $ txInInfoResolved inp)
              )
              inputs
          -- verifier que le lp a le montant de token qu'il fournit
          goodTokenInput =
            List.any
              ( \inp ->
                  List.any
                    (\(cs, _, am) -> (cs P.== tokSymb) P.&& (am P.>= tokenAmount))
                    (flattenValue $ txOutValue $ txInInfoResolved inp)
              )
              inputs
       in goodAdaInput P.&& goodTokenInput

    -- Outputs
    -- verifier que l'UTXO du script a été bien mis a jour
    -- avec le montant d'Ada et de token ajoutés aux reserves deja existantes
    scriptOuput :: AdaReserve -> TokenReserve -> LPTokenCurrencySymbol -> ScriptContext -> Bool
    scriptOuput adaR tokR tokSymb ctx =
      case getContinuingOutputs ctx of
        [o] ->
          let values = flattenValue (txOutValue o)
              trueAdaAmount = List.any (\(cs, _, am) -> (cs P.== adaSymbol) P.&& (am P.== adaR P.+ adaAmount)) values
              trueTokenAMount = List.any (\(cs, _, am) -> (cs P.== tokSymb) P.&& (am P.== tokR P.+ tokenAmount)) values
              newDatumHash = getHash (adaAmount P.+ adaR) (tokenAmount P.+ tokR)
              goodDatumHash = case txOutDatum o of
                OutputDatumHash (DatumHash hash) -> newDatumHash == hash
                _ -> False
           in trueAdaAmount P.&& trueTokenAMount P.&& goodDatumHash
        -- lpOuput correspond à l'output correspondant
        -- a celui dans lequel le LP a recu les LP tokens ou tokensForLp
        _ -> False
    lpOutput :: PoolParams -> ScriptContext -> Bool
    lpOutput poolParams ctx =
      List.any
        ( \o ->
            valueOf (txOutValue o) (lpCurrSymb poolParams) (lpTokName poolParams)
              == tokensForLp
              P.&& ( case addressCredential (txOutAddress o) of
                       PubKeyCredential ownerPKH -> ownerPKH == (lpPKH poolParams)
                       _ -> False
                   )
        )
        (txInfoOutputs (scriptContextTxInfo ctx))

{-# INLINEABLE getHash #-}
getHash :: AdaReserve -> TokenReserve -> BuiltinByteString
getHash adaR tokR =
  let adaHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 adaR)
      tokHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 tokR)
      finalHash = Builtins.sha2_256 $ Builtins.appendByteString adaHash tokHash
   in finalHash

--swap part
{-# INLINEABLE swap #-}
--Take 1% on each swap
-- Before swap we must verify off-chain if there is enough of tokens for the
-- swap
swap:: Amount -> Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
-- amount -> amount to swap
-- minAmount -> minimum token expected
-- tokSymbol == adaSymbol -> that mean the user want to swap ada to token
swap amount minAmount adaR tokR tokSymbol ctx =
    List.and conditions
  -- swaps ada with tokens
    where
     conditions:: [Bool]
     conditions = [
                     userInput amount tokSymbol ctx,
                     userOutput amount minAmount adaR tokR tokSymbol ctx,
                     scriptOutput amount adaR tokR tokSymbol ctx
                    ]
    --tokSymbol can be adaSymbol or the TokenSymbol. It depends on what user wants to swap.
     userInput:: Amount -> TokenSymbol -> ScriptContext -> Bool
     userInput amount tokSymbol ctx =
      let userPKH = List.head $ txInfoSignatories (scriptContextTxInfo ctx)
          inputs = txInfoInputs $ scriptContextTxInfo ctx
          --find the good input at the user pkh with the good amount
       in List.any(\inp ->
              case addressCredential (txOutAddress $ txInInfoResolved inp) of
                       PubKeyCredential pkh -> pkh == userPKH
                                               P.&&

                                               List.any (\(cs, _, am) -> (cs P.== tokSymbol) P.&& (am P.>= amount))
                                                  (flattenValue $ txOutValue $ txInInfoResolved inp)
                       _ -> False
            ) inputs

     userOutput:: Amount -> Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
     userOutput amount minAmount adaR tokR tokSymbol ctx =
          case tokSymbol P.== adaSymbol of
              True -> let tokensBougth = adaToToken amount adaR tokR
                          pkh = List.head $ txInfoSignatories (scriptContextTxInfo ctx)
                       in if tokensBougth < minAmount
                             then False
                            -- Verify that user get the good tokens amount
                            -- in output
                             else List.any ( \o ->
                                        List.any
                                          (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokensBougth))
                                          (flattenValue $ txOutValue $ o)
                                          P.&& ( case addressCredential (txOutAddress o) of
                                                   PubKeyCredential userPKH -> userPKH P.== pkh
                                                   _ -> False
                                                )
                                          )
                                          (txInfoOutputs (scriptContextTxInfo ctx))

              False -> let adaBougth = tokenToAda amount tokR adaR
                           userPKH = List.head $ txInfoSignatories (scriptContextTxInfo ctx)
                        in if adaBougth < minAmount
                             then False
                            -- Verify that user get the good tokens amount
                            -- in output
                             else List.any ( \o ->
                                    valueOf (txOutValue o) adaSymbol adaToken P.== adaBougth
                                          P.&& ( case addressCredential (txOutAddress o) of
                                                   PubKeyCredential pkh -> pkh == userPKH
                                                   _ -> False
                                                )
                                          )
                                          (txInfoOutputs (scriptContextTxInfo ctx))

     scriptOutput:: Amount -> AdaReserve -> TokenReserve -> TokenSymbol -> ScriptContext -> Bool
     scriptOutput amount adaR tokR tokSymbol ctx =
        case getContinuingOutputs ctx of
           [o] -> case tokSymbol P.== adaSymbol of
                      True -> let tokensBougth = adaToToken amount adaR tokR
                      --update the ada's reserve because it swap ada and get tokens
                      -- so that token's reserve decrease and ada's reserve increase
                                  adaReserveUpdate =  valueOf (txOutValue o) adaSymbol adaToken P.== adaR P.+ amount
                                  tokenReserveUpdate = List.any
                                                        (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokR P.- tokensBougth))
                                                        (flattenValue $ txOutValue $ o)
                                in adaReserveUpdate P.&& tokenReserveUpdate
                      False -> let adaBougth = tokenToAda amount tokR adaR
                      --update the token's reserve because it gives tokens and get ada
                      -- so that token's reserve increase and ada's reserve decrease
                                   adaReserveUpdate =  valueOf (txOutValue o) adaSymbol adaToken P.== adaR P.- adaBougth
                                   tokenReserveUpdate = List.any
                                                        (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokR P.+ amount))
                                                        (flattenValue $ txOutValue $ o)
                                in adaReserveUpdate P.&& tokenReserveUpdate
           _ -> False






{-# INLINEABLE adaToToken #-}
adaToToken:: Amount -> AdaReserve -> TokenReserve -> Amount
adaToToken amount adaR tokR =
   let tokensBougth = getAmountForSwp amount adaR tokR
    in tokensBougth

{-# INLINEABLE tokenToAda #-}
tokenToAda:: Amount -> TokenReserve -> AdaReserve -> Amount
tokenToAda amount tokR adaR =
 let adaBougth = getAmountForSwp amount tokR adaR
  in adaBougth


{-# INLINEABLE getAmountForSwp #-}
--get 1% of fee on each swap
getAmountForSwp:: Amount -> Amount -> Amount -> Amount
getAmountForSwp inputAmount inputReserve outputReserve =
  let inputAmountWithFee = inputAmount P.* 99
      numerator = inputAmountWithFee * outputReserve
      denominator = (inputReserve P.* 100) P.+ inputAmountWithFee
    in P.divide numerator denominator


--check the input to get
ammUntypedValidator :: BuiltinData -> BuiltinData -> BuiltinData -> P.BuiltinUnit
ammUntypedValidator _ redeemer ctx =
  -- check retourne ()
  P.check
    ( ammValidator
        (PlutusTx.unsafeFromBuiltinData redeemer)
        (PlutusTx.unsafeFromBuiltinData ctx)
    )

ammValidatorScript :: CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> P.BuiltinUnit)
ammValidatorScript = $$(PlutusTx.compile [||ammUntypedValidator||])
