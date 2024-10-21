module Compilation (compileTerm) where

import           Data.Text (Text)

import           Plutarch  (ClosedTerm, Config (Tracing), LogLevel (LogInfo),
                            PType, Script, TracingMode (DetTracing), compile)

compileTerm :: forall {a :: PType}. ClosedTerm a -> Either Text Script
compileTerm = compile (Tracing LogInfo DetTracing)
