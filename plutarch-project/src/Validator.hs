module Validator (pvalidator) where

import           Plutarch.LedgerApi.V3 (PScriptContext)
import           Plutarch.Prelude

pvalidator :: Term s (PScriptContext :--> PUnit)
pvalidator = phoistAcyclic $ plam $ \_ctx -> pconstant ()
