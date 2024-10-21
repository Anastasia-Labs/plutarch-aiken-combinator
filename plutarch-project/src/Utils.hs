module Utils (writePlutusScript) where

import           Data.Aeson               (KeyValue ((.=)), object)
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Bifunctor           (first)
import qualified Data.ByteString.Base16   as Base16
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text                (Text, pack)
import qualified Data.Text.Encoding       as Text

import qualified Cardano.Binary           as CBOR
import           PlutusLedgerApi.V2       (Data, ExBudget)

import           Plutarch.Evaluate        (applyArguments, evalScript)
import           Plutarch.Prelude
import           Plutarch.Script          (Script, serialiseScript)

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

writePlutusScript :: String -> FilePath -> ClosedTerm a -> IO ()
writePlutusScript title filepath term = do
  case evalT term of
    Left e -> print e
    Right (script, _, _) -> do
      let scriptType = "PlutusScriptV3" :: String
          plutusJson = object ["type" .= scriptType, "description" .= title, "cborHex" .= encodeSerialiseCBOR script]
          content = encodePretty plutusJson
      LBS.writeFile filepath content
