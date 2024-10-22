module Main (main) where

import           System.Console.ANSI (Color (..), ColorIntensity (..),
                                      ConsoleLayer (..), SGR (..), setSGR)

import           Constants           (aikenFileName, compiledDirectory)
import           Utils               (writePlutusScript,
                                      writeScriptHashAsAikenConstant)
import           Validator           (pvalidator)


aikenEnvDirectory :: FilePath
aikenEnvDirectory = "../aiken-project/env/"

main :: IO ()
main = do
  setSGR [SetColor Foreground Vivid Blue]
  putStrLn "Exporting..."
  setSGR [Reset]

  let aikenConstantFile = aikenEnvDirectory <> aikenFileName <> ".ak"
  writeScriptHashAsAikenConstant
    "observer_credential"
    aikenConstantFile
    pvalidator
  putStrLn "Exported staking validator hash as Aiken constant into:"
  putStrLn $ "\t" <> aikenConstantFile

  let scriptJsonFile = compiledDirectory <> "stakingScript.json"
  writePlutusScript
    "staking_script_hash"
    scriptJsonFile
    pvalidator
  putStrLn "Exported staking validator script JSON into:"
  putStrLn $ "\t" <> scriptJsonFile

  setSGR [SetColor Foreground Vivid Green]
  putStrLn "Export complete!"
  setSGR [Reset]
