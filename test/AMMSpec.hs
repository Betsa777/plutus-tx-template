module Main (main) where

import AMM (AMMRedeemer (..), ammValidator)
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

adaR :: Integer
adaR = 200

tokR :: Integer
tokR = 0

lpTokNumb :: Integer
lpTokNumb = 10

lpPKH :: PubKeyHash
lpPKH = PubKeyHash $ BuiltinByteString (C.pack "lp")

lpCurrSymb :: CurrencySymbol
lpCurrSymb = CurrencySymbol $ BuiltinByteString (C.pack "LP")

lpTokName :: TokenName
lpTokName = TokenName $ BuiltinByteString (C.pack "LPToken")

tokCS :: CurrencySymbol
tokCS = CurrencySymbol $ BuiltinByteString (C.pack "WIMS")

tokName :: TokenName
tokName = TokenName $ BuiltinByteString (C.pack "WimsToken")

-- Remplace ScriptHash par ValidatorHash
scriptAddress = Address (ScriptCredential (ScriptHash (BuiltinByteString (C.pack "script")))) Nothing

lpAddress = Address (PubKeyCredential (PubKeyHash (BuiltinByteString (C.pack "lp")))) Nothing

previousDatumHash =
  let adaHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 adaR)
      tokHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 tokR)
    in Builtins.sha2_256 $ Builtins.appendByteString adaHash tokHash

newDatumHash =
 let adaHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (adaAmount + adaR))
     tokHash = Builtins.sha2_256 (Builtins.integerToByteString Builtins.BigEndian 0 (tokenAmount + tokR))
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
            <> singleton tokCS tokName tokenAmount,
          txOutDatum = NoOutputDatum,
          txOutReferenceScript = Nothing
        }
}
scriptOutput :: TxOut
scriptOutput =
  TxOut
    { txOutAddress = scriptAddress,
      txOutValue =
        singleton adaSymbol adaToken (adaAmount + adaR)
          <> singleton tokCS tokName (tokenAmount + tokR) ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

value = singleton adaSymbol adaToken adaAmount

-- comme tokR == 0 donc tokensForLp = adaR + adaAmount
lpOutput =
  TxOut
    { txOutAddress = lpAddress,
      txOutValue = singleton lpCurrSymb lpTokName (adaR + adaAmount),
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
      txInfoSignatories = [lpPKH],
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
redeemer = AddLiquidity adaAmount tokenAmount adaR tokR lpTokNumb lpPKH lpCurrSymb lpTokName

main :: IO ()
main = hspec $ do
  describe "AMMValidator" $ do
    it "must give true" $ do
      let [o] = getContinuingOutputs scriptContext
      putStrLn $ show $ txOutValue o
      ammValidator redeemer scriptContext
        `shouldBe` True
