module Utils (writePlutusScript, writeScriptHashAsAikenConstant) where

import           Data.Aeson                 (KeyValue ((.=)), object)
import           Data.Aeson.Encode.Pretty   (encodePretty)
import           Data.Bifunctor             (first)
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Lazy       as LBS
import           Data.Text                  (Text, pack)
import qualified Data.Text.Encoding         as Text

import qualified Cardano.Binary             as CBOR
import           PlutusLedgerApi.V2         (Data, ExBudget)
import           PlutusTx.Builtins.Internal (BuiltinByteString (..))

import           Data.ByteString            (ByteString)

import           Plutarch.Evaluate          (applyArguments, evalScript)
import qualified Plutarch.LedgerApi.V3      as V3
import           Plutarch.Prelude
import           Plutarch.Script            (Script, serialiseScript)
import qualified PlutusLedgerApi.Data.V1    as V1

import           Compilation

encodeSerialiseCBOR :: Script -> Text
encodeSerialiseCBOR = Text.decodeUtf8 . Base16.encode . CBOR.serialize' . serialiseScript

evalT :: ClosedTerm a -> Either Text (Script, ExBudget, [Text])
evalT x = evalWithArgsT x []

evalWithArgsT :: ClosedTerm a -> [Data] -> Either Text (Script, ExBudget, [Text])
evalWithArgsT x args = do
  cmp <- compileTerm x
  let (escr, budg, trc) = evalScript $ applyArguments cmp args
  scr <- first (pack . show) escr
  pure (scr, budg, trc)


compileToScriptHash :: ClosedTerm a -> Either Text ByteString
compileToScriptHash t =
  case compileTerm t of
    Right compiled ->
      case V1.getScriptHash (V3.scriptHash compiled) of
        BuiltinByteString bs -> Right bs
    Left e -> Left e


writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> print e
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV3" :: String
          plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
          content = encodePretty plutusJson
      LBS.writeFile filepath content

writeScriptHashAsAikenConstant :: Text -> FilePath -> ClosedTerm a -> IO ()
writeScriptHashAsAikenConstant constantName filepath term =
  case compileToScriptHash term of
    Right hash ->
      let
        content = Text.decodeUtf8 $ Base16.encode hash
      in
      LBS.writeFile filepath
        $ LBS.fromStrict
        $ Text.encodeUtf8
        $ "const " <> constantName <> ": ByteArray = #\"" <> content <> "\""
    Left e -> print e
