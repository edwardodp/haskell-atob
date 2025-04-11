{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import System.Process (readProcess)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- Dynamically get the path to the built executable
  exeBase <- readProcess "stack" ["path", "--local-install-root"] ""
  let exePath = init exeBase </> "bin" </> "haskell-atob-exe"  -- `init` removes trailing newline

  hspec $ do
    describe "A=B Interpreter Integration Tests" $ do
      testFiles <- runIO $ listDirectory "test/cases"
      forM_ testFiles $ \testFile -> do
        let baseName = takeBaseName testFile
            codePath = "code" </> baseName
            casePath = "test/cases" </> testFile

        it ("runs program: " ++ baseName) $ do
          contents <- TIO.readFile casePath
          let lines' = T.lines contents
              testPairs = toPairs lines'

          forM_ testPairs $ \(input, expected) -> do
            output <- readProcess exePath [codePath, T.unpack input] ""
            T.strip (T.pack output) `shouldBe` T.strip expected

-- Converts [a, b, c, d] -> [(a, b), (c, d)]
toPairs :: [T.Text] -> [(T.Text, T.Text)]
toPairs (x:y:rest) = (x, y) : toPairs rest
toPairs _ = []
