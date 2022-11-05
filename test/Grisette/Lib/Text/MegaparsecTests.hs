{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Grisette.Lib.Text.MegaparsecTests where

import Grisette.Core.Control.Monad.UnionMBase
import Grisette.Core.Data.Class.Bool
import Grisette.Core.Data.Class.GenSym
import Grisette.Core.Data.Class.Mergeable
import Grisette.Core.Data.Class.SimpleMergeable
import Grisette.Lib.Text.Megaparsec ()
import Grisette.TestUtils.SBool
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec hiding (single)
import Text.Megaparsec.Char

type Parser = ParsecT String String (UnionMBase SBool)

type ParserS = ParsecT String String GenSymFresh

wrapParser :: Monad m => m String -> m SBool
wrapParser parser = SSBool <$> parser

p :: MonadParsec e String m => String -> m SBool
p = wrapParser . string

megaparsecTests :: TestTree
megaparsecTests =
  testGroup
    "MegaparsecTests"
    [ testGroup
        "ParsecT"
        [ testCase "SimpleMergeable" $ do
            let p1 = do
                  a <- p "a"
                  ba <- p "ba"
                  return $ a &&~ ba
            let p2 = do
                  ab <- p "ab"
                  a <- p "a"
                  return $ ab ||~ a
            let r = gmrgIte (SSBool "a") p1 p2 :: Parser SBool
            let r1 = gmrgIte1 (SSBool "a") p1 p2 :: Parser SBool
            let ru1 = mrgIf (SSBool "a") p1 p2 :: Parser SBool
            runParserT r "x" "aba"
              @=? unionIf
                (SSBool "a")
                (single (Right (And (SSBool "a") (SSBool "ba"))))
                (single (Right (Or (SSBool "ab") (SSBool "a"))))
            runParserT r1 "x" "aba"
              @=? unionIf
                (SSBool "a")
                (single (Right (And (SSBool "a") (SSBool "ba"))))
                (single (Right (Or (SSBool "ab") (SSBool "a"))))
            runParserT ru1 "x" "aba"
              @=? unionIf
                (SSBool "a")
                (single (Right (And (SSBool "a") (SSBool "ba"))))
                (single (Right (Or (SSBool "ab") (SSBool "a")))),
          testCase "Mergeable" $ do
            let SimpleStrategy s = gmergingStrategy :: GMergingStrategy SBool (Parser SBool)
            let p1 = do
                  a <- p "a"
                  ba <- p "ba"
                  return $ a &&~ ba
            let p2 = do
                  ab <- p "ab"
                  a <- p "a"
                  return $ ab ||~ a
            let r = s (SSBool "a") p1 p2
            runParserT r "x" "aba"
              @=? unionIf
                (SSBool "a")
                (single (Right (And (SSBool "a") (SSBool "ba"))))
                (single (Right (Or (SSBool "ab") (SSBool "a")))),
          testGroup
            "MonadUnion"
            [ testCase "single" $ do
                runParserT (single (SSBool "a") :: Parser SBool) "x" ""
                  @=? single (Right (SSBool "a")),
              testCase "unionIf" $ do
                let p1 = do
                      a <- p "a"
                      ba <- p "ba"
                      return $ a &&~ ba
                let p2 = do
                      ab <- p "ab"
                      a <- p "a"
                      return $ ab ||~ a
                let r = unionIf (SSBool "a") p1 p2 :: Parser SBool
                runParserT r "x" "aba"
                  @=? unionIf
                    (SSBool "a")
                    (single (Right (And (SSBool "a") (SSBool "ba"))))
                    (single (Right (Or (SSBool "ab") (SSBool "a"))))
            ],
          testGroup
            "MonadGenSymFresh"
            [ testCase "gen" $ do
                let p1 = do
                      _ <- p "a"
                      genSymSimpleFresh ()
                let p2 = do
                      v1 <- p1
                      v2 <- p1
                      return $ v1 &&~ v2
                runGenSymFresh (runParserT (p2 :: ParserS SBool) "x" "aa") "name"
                  @=? Right (And (ISBool "name" 0) (ISBool "name" 1))
            ]
        ]
    ]
