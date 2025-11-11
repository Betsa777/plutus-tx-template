module Main (main) where

import AMM (AMMRedeemer (..), PoolParams(..),ammValidator,adaToToken,tokenToAda)
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

--AddLiquidity
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

--Swap ada with token
--
--
amount = 1000
minAmount = 100
swapAdaR = 2000
swapTokR = 4000
swapTokSymbol = CurrencySymbol $ BuiltinByteString (C.pack "WIMS")

swapScriptInput = TxInInfo {
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = scriptAddress,
          txOutValue =
            singleton adaSymbol adaToken swapAdaR
            <> singleton swapTokSymbol tokName swapTokR,
            --passer a la verification par DatumHash
            -- etre sur que les reserves passes sont les bonnes
          txOutDatum = OutputDatumHash $ DatumHash previousDatumHash,
          txOutReferenceScript = Nothing
        }
}

userPKH= PubKeyHash (BuiltinByteString (C.pack "userswap"))
userSwapAddress = Address (PubKeyCredential userPKH) Nothing

swapUserInput:: TxInInfo
swapUserInput = TxInInfo{
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = userSwapAddress,
          txOutValue = singleton adaSymbol adaToken amount,
          txOutDatum = NoOutputDatum,
          txOutReferenceScript = Nothing
        }
}


tokensBougth = adaToToken amount swapAdaR swapTokR
--good script with adaR + newAdaAmount
-- good script output with tokR - tokenGotByTheUser
scriptSwapOutput =
  TxOut
    { txOutAddress = scriptAddress,
      txOutValue =
        singleton adaSymbol adaToken (amount + swapAdaR)
          <> singleton swapTokSymbol tokName (swapTokR - tokensBougth) ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

userSwapOutput =
  TxOut
    { txOutAddress = userSwapAddress,
      txOutValue = singleton swapTokSymbol tokName tokensBougth ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

swapTxInfo:: TxInfo
swapTxInfo = TxInfo {
      txInfoInputs = [swapScriptInput ,swapUserInput],
      txInfoReferenceInputs = [],
      txInfoOutputs = [scriptSwapOutput, userSwapOutput],
      txInfoFee = value,
      txInfoMint = value,
      txInfoDCert = [],
      txInfoWdrl = Map.empty,
      txInfoValidRange = interval 10 20,
      txInfoSignatories = [userPKH],
      txInfoRedeemers = Map.empty,
      txInfoData = Map.empty,
      txInfoId = TxId $ BuiltinByteString (C.pack "")
}
swapScriptContext :: ScriptContext
swapScriptContext =
  ScriptContext
    { scriptContextTxInfo = swapTxInfo,
      scriptContextPurpose = Spending $ TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0
    }
swapRedeemer:: AMMRedeemer
swapRedeemer = Swap amount minAmount swapAdaR swapTokR adaSymbol

--Swap token with ada
--
--
amount' = 100
minAmount' = 10
swapAdaR' = 100
swapTokR' = 250
swapTokSymbol' = CurrencySymbol $ BuiltinByteString (C.pack "WIMS")

swapScriptInput' = TxInInfo {
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = scriptAddress,
          txOutValue =
            singleton adaSymbol adaToken swapAdaR'
            <> singleton swapTokSymbol' tokName swapTokR',
            --passer a la verification par DatumHash
            -- etre sur que les reserves passes sont les bonnes
          txOutDatum = OutputDatumHash $ DatumHash previousDatumHash,
          txOutReferenceScript = Nothing
        }
}


swapUserInput':: TxInInfo
swapUserInput' = TxInInfo{
    txInInfoOutRef =  TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0,
    txInInfoResolved =
      TxOut
        { txOutAddress = userSwapAddress,
          txOutValue = singleton swapTokSymbol' tokName amount',
          txOutDatum = NoOutputDatum,
          txOutReferenceScript = Nothing
        }
}


adaBougth = tokenToAda amount' swapTokR' swapAdaR'
--good script with adaR + newAdaAmount
-- good script output with tokR - tokenGotByTheUser
scriptSwapOutput' =
  TxOut
    { txOutAddress = scriptAddress,
      txOutValue =
        singleton adaSymbol adaToken (swapAdaR' - adaBougth)
          <> singleton swapTokSymbol' tokName (swapTokR' + amount') ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

userSwapOutput' =
  TxOut
    { txOutAddress = userSwapAddress,
      txOutValue = singleton adaSymbol adaToken adaBougth ,
      txOutDatum = OutputDatumHash $ DatumHash newDatumHash,
      txOutReferenceScript = Nothing
    }

swapTxInfo':: TxInfo
swapTxInfo' = TxInfo {
      txInfoInputs = [swapScriptInput' ,swapUserInput'],
      txInfoReferenceInputs = [],
      txInfoOutputs = [scriptSwapOutput', userSwapOutput'],
      txInfoFee = value,
      txInfoMint = value,
      txInfoDCert = [],
      txInfoWdrl = Map.empty,
      txInfoValidRange = interval 10 20,
      txInfoSignatories = [userPKH],
      txInfoRedeemers = Map.empty,
      txInfoData = Map.empty,
      txInfoId = TxId $ BuiltinByteString (C.pack "")
}
swapScriptContext' :: ScriptContext
swapScriptContext' =
  ScriptContext
    { scriptContextTxInfo = swapTxInfo',
      scriptContextPurpose = Spending $ TxOutRef (TxId $ BuiltinByteString (C.pack "")) 0
    }
swapRedeemer':: AMMRedeemer
swapRedeemer' = Swap amount' minAmount' swapAdaR' swapTokR' swapTokSymbol'
main :: IO ()
main = hspec $ do
  describe "AMMValidator" $ do
    it "addliquidty test if token reserve equals 0" $ do
      let [o] = getContinuingOutputs scriptContext
      putStrLn $ show $ txOutValue o
      ammValidator redeemer scriptContext
        `shouldBe` True
    it "swaps ada with wims token must succeed" $ do
      putStrLn $ "TokenBougth is " ++ show tokensBougth
      ammValidator swapRedeemer swapScriptContext
        `shouldBe` True
    it "swaps wims token to ada must succeed" $ do
      putStrLn $ "AdaBougth is " ++ show adaBougth
      ammValidator swapRedeemer' swapScriptContext'
       `shouldBe` True
