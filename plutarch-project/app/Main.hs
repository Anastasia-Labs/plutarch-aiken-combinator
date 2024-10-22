module Main (main) where

import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleLayer (..), SGR (..), setSGR)
import           Utils               (writeScriptHashAsAikenConstant)
import           Validator           (pvalidator)

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "Exporting Plutarch scripts..."
  setSGR [Reset]

  writeScriptHashAsAikenConstant "test_script_hash" "../aiken-project/env/test.ak" pvalidator
  putStrLn "Exported smart handle router validator"

  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Done exporting Plutarch scripts, have a great day!"
  setSGR [Reset]
