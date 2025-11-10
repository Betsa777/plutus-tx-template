module Main (main) where

import AMM (AMMRedeemer (..), PoolParams(..),ammValidator)
import qualified Data.ByteString.Char8  as C
import PlutusLedgerApi.V1 (TxId (..))
import PlutusLedgerApi.V1.Interval (interval)
import PlutusLedgerApi.V2.Contexts (getContinuingOutputs)
import PlutusLedgerApi.V1.Value
  ( CurrencySymbol (..),
    TokenName (..),
    adaSymbol,
    adaToken,
    singleton,
  )
import PlutusLedgerApi.V2
  ( Address (..),
    Credential (..),
    OutputDatum (..),
    DatumHash(..),
    PubKeyHash (..),
    ScriptContext (..),
    ScriptHash (..),
    addressCredential,
    scriptContextTxInfo,
  )
import PlutusLedgerApi.V2.Contexts
  ( ScriptPurpose (..),
    TxInfo (..),
    TxOut (..),
    TxOutRef (..),
    TxInInfo(..)
  )
import qualified PlutusTx.AssocMap as Map
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Builtins as Builtins
import Test.Hspec

adaAmount :: Integer
adaAmount = 100

tokenAmount :: Integer
tokenAmount = 200

poolParams:: PoolParams
poolParams = PoolParams{
    adaR = 200,
    tokR = 0,
    tokSymbol = CurrencySymbol $ BuiltinByteString (C.pack "WIMS"),
    lpTokNumb = 10,
    lpPKH = PubKeyHash $ BuiltinByteString (C.pack "lp"),
    lpCurrSymb = CurrencySymbol $ BuiltinByteString (C.pack "LP"),
    lpTokName = TokenName $ BuiltinByteString (C.pack "LPToken")
}
tokName :: TokenName
tokName = TokenName $ BuiltinByteString (C.pack "WimsToken")

-- Remplace ScriptHash par ValidatorHash
scriptAddress = Address (ScriptCredential (ScriptHash (BuiltinByteString (C.pack "script")))) Nothing

lpAddress = Address (PubKeyCredential (PubKeyHash (BuiltinByteString (C.pack "lp")))) Nothing

previousDatumHash =
  let adaHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (adaR poolParams))
      tokHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (tokR poolParams))
    in Builtins.sha2_256 $ Builtins.appendByteString adaHash tokHash

newDatumHash =
 let adaHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (adaAmount + (adaR poolParams)))
     tokHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (tokenAmount + (tokR poolParams)))
  in Builtins.sha2_256 $ Builtins.appendByteString adaHash tokHash

scriptInput:: TxInInfo
scriptInput = TxInInfo{
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = scriptAddress,
          txOutValue =
            singleton adaSymbol adaToken 0,
            --passer a la verification par DatumHash
            -- etre sur que les reserves passes sont les bonnes
          txOutDatum = OutputDatumHash $ DatumHash previousDatumHash,
          txOutReferenceScript = Nothing
        }
}
lpInput:: TxInInfo
lpInput = TxInInfo{
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = lpAddress,
          txOutValue = singleton adaSymbol adaToken adaAmount
            <> singleton (tokSymbol poolParams) tokName tokenAmount,
          txOutDatum = NoOutputDatum,
          txOutReferenceScript = Nothing
        }
}
scriptOutput :: TxOut
scriptOutput =
  TxOut
    { txOutAddress = scriptAddress,
      txOutValue =
        singleton adaSymbol adaToken (adaAmount + (adaR poolParams))
          <> singleton (tokSymbol poolParams) tokName (tokenAmount + (tokR poolParams)) ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

value = singleton adaSymbol adaToken adaAmount

-- comme tokR == 0 donc tokensForLp = adaR + adaAmount
lpOutput =
  TxOut
    { txOutAddress = lpAddress,
      txOutValue = singleton (lpCurrSymb poolParams) (lpTokName poolParams) ((adaR poolParams) + adaAmount),
      txOutDatum = NoOutputDatum,
      txOutReferenceScript = Nothing
    }

txInfo :: TxInfo
txInfo =
  TxInfo
    { txInfoInputs = [scriptInput,lpInput],
      txInfoReferenceInputs = [],
      txInfoOutputs = [lpOutput, scriptOutput],
      txInfoFee = value,
      txInfoMint = value,
      txInfoDCert = [],
      txInfoWdrl = Map.empty,
      txInfoValidRange = interval 10 20,
      txInfoSignatories = [lpPKH poolParams],
      txInfoRedeemers = Map.empty,
      txInfoData = Map.empty,
      txInfoId = TxId $ BuiltinByteString (C.pack "")
    }

scriptContext :: ScriptContext
scriptContext =
  ScriptContext
    { scriptContextTxInfo = txInfo,
      scriptContextPurpose = Spending $ TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0
    }

redeemer :: AMMRedeemer
redeemer = AddLiquidity adaAmount tokenAmount poolParams

main :: IO ()
main = hspec $ do
  describe "AMMValidator" $ do
    it "addliquidty test if token reserve equals 0" $ do
      let [o] = getContinuingOutputs scriptContext
      putStrLn $ show $ txOutValue o
      ammValidator redeemer scriptContext
        `shouldBe` True
