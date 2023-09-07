{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use fmap" #-}
module Main where

import Ast
import Control.Monad
import Data.Either (fromRight)
import Debug.Trace (trace)
import Qasm2 qualified
import Qasm3
import Qasm3Arbitrary qualified as Q3A
import Qasm3Parser qualified as Q3P
import System.Exit (exitFailure)
import Test.HUnit
import Test.QuickCheck
import System.IO

testAstEquivalence = TestLabel "AST Equivalence" $ TestCase $ do
  -- ast = AstNode ...

  genAst <- generate (Q3A.arbitraryProgramNode cfg)

  putStrLn $ "Original AST:\n" ++ show genAst ++ "\n"
  hFlush stdout
  let str = pretty genAst
  putStrLn $ "Generated source:\n" ++ str ++ "\n"
  hFlush stdout
  let parseResult = (Right . syntaxTreeFrom) =<< Q3P.parseString str
  let ast = syntaxTreeFrom genAst
  putStrLn $ "Parse result:\n" ++ show parseResult ++ "\n"
  hFlush stdout

  assertBool "Round-Trip AST Equivalent" (Right ast == parseResult)

tests =
  TestList
    [ testAstEquivalence
    ]

cfg = Q3A.defaultConfig

prop_roundTrip = forAll (Q3A.arbitraryProgramNode cfg) $
  \ast -> syntaxTreeFrom ast == syntaxTreeFrom (fromRight NilNode $ Q3P.parseString (pretty ast))

main :: IO ()
main = do
  count <- runTestTT tests
  unless (failures count == 0) exitFailure

  result <- verboseCheckResult (withMaxSuccess 10000 prop_roundTrip)
  unless (isSuccess result) exitFailure
