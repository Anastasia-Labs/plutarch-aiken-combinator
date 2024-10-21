module Main (main) where

import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleLayer (..), SGR (..), setSGR)
import           Utils               (writePlutusScript)
import           Validator           (pvalidator)

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "Exporting Plutarch scripts..."
  setSGR [Reset]

  writePlutusScript "Smart Handle Router" "../aiken-project/env/test.json" pvalidator
  putStrLn "Exported smart handle router validator"

  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Done exporting Plutarch scripts, have a great day!"
  setSGR [Reset]
