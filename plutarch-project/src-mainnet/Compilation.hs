module Compilation (compileTerm) where

import           Data.Text (Text)

import           Plutarch  (ClosedTerm, Config (NoTracing), PType, Script,
                            compile)

compileTerm :: forall {a :: PType}. ClosedTerm a -> Either Text Script
compileTerm = compile NoTracing
