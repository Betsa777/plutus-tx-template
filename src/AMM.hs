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

-- import Plutus.V1.Ledger.Api ()

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

type LPTokenName = TokenName

data AMMRedeemer
  = AddLiquidity Amount Amount AdaReserve TokenReserve LPTokenNumber LPPKH LPTokenCurrencySymbol LPTokenName
  | Swap Amount LPPKH
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
    AddLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ->
      P.traceIfFalse "AddLiquidty fails" (addLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ctx)

-- Swap tA tB amount owner ->
--   P.traceIfFalse "Swap fails" (swap tA tB amount owner ctx)
-- RemoveLiquidity tA tB amount owner ->
--   P.traceIfFalse "Removal fails" (removeLiquidity tA tB amount owner ctx)

-- ajout de liquidité

{-# INLINEABLE addLiquidity #-}
addLiquidity :: Amount -> Amount -> AdaReserve -> TokenReserve -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
addLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName ctx =
  if adaAmount <= 0 || tokenAmount <= 0
    then False
    else
      -- au cas ou il n'ya pas de reserve du token ajouté
      if tokR == 0
        then
          let tokensForLp = adaR + adaAmount
           in finalizeAdding adaAmount tokenAmount adaR tokR tokensForLp lpPKH lpCurrSymb lpTokName ctx
        else
          let tokensForLp = P.divide (lpTokNumb P.* adaAmount) adaR
           in finalizeAdding adaAmount tokenAmount adaR tokR tokensForLp lpPKH lpCurrSymb lpTokName ctx

{-# INLINEABLE finalizeAdding #-}
finalizeAdding :: Amount -> Amount -> AdaReserve -> TokenReserve -> LPTokenNumber -> LPPKH -> LPTokenCurrencySymbol -> LPTokenName -> ScriptContext -> Bool
finalizeAdding adaAmount tokenAmount adaR tokR tokensForLp lpPKH lpCurrSymb lpTokName ctx =
  List.and [goodReserves adaR tokR ctx, lpInput ctx, scriptOuput adaR tokR ctx, lpOutput ctx]
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
    lpInput :: ScriptContext -> Bool
    lpInput ctx =
      let inputs = txInfoInputs $ scriptContextTxInfo ctx
          -- verifier que le lp a le montant d'Ada qu'il fournit
          goodAdaInput =
            List.any
              ( \inp ->
                  List.any
                    (\(cs, _, am) -> (cs P.== adaSymbol) P.&& (am P.== adaAmount))
                    (flattenValue $ txOutValue $ txInInfoResolved inp)
              )
              inputs
          -- verifier que le lp a le montant de token qu'il fournit
          goodTokenInput =
            List.any
              ( \inp ->
                  List.any
                    (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokenAmount))
                    (flattenValue $ txOutValue $ txInInfoResolved inp)
              )
              inputs
       in goodAdaInput P.&& goodTokenInput

    -- Outputs
    -- verifier que l'UTXO du script a été bien mis a jour
    -- avec le montant d'Ada et de token ajoutés aux reserves deja existantes
    scriptOuput :: AdaReserve -> TokenReserve -> ScriptContext -> Bool
    scriptOuput adaR tokR ctx =
      case getContinuingOutputs ctx of
        [o] ->
          let values = flattenValue (txOutValue o)
              trueAdaAmount = List.any (\(cs, _, am) -> (cs P.== adaSymbol) P.&& (am P.== adaR P.+ adaAmount)) values
              trueTokenAMount = List.any (\(cs, _, am) -> (cs P./= adaSymbol) P.&& (am P.== tokR P.+ tokenAmount)) values
              newDatumHash = getHash (adaAmount P.+ adaR) (tokenAmount P.+ tokR)
              goodDatumHash = case txOutDatum o of
                OutputDatumHash (DatumHash hash) -> newDatumHash == hash
                _ -> False
           in trueAdaAmount P.&& trueTokenAMount P.&& goodDatumHash
        -- lpOuput correspond à l'output correspondant
        -- a celui dans lequel le LP a recu les LP tokens ou tokensForLp
        _ -> False
    lpOutput :: ScriptContext -> Bool
    lpOutput ctx =
      List.any
        ( \o ->
            valueOf (txOutValue o) lpCurrSymb lpTokName
              == tokensForLp
              P.&& ( case addressCredential (txOutAddress o) of
                       PubKeyCredential ownerPKH -> ownerPKH == lpPKH
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
