{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS -fno-warn-orphans #-}

module BMake.Base
  ( thenP
  , returnP
  , happyError
  , Parser
  , Token(..)
  , TokenClass(..)
  , AlexState(..)
  , lexer
  , UnitF(..)
  , Unit
  , StatementF(..)
  , Statement
  , Expr
  , ExprOne
  , ExprOneF(..)
  , module BMake.Lexer
  )
  where

--------------------------------------------------------------------------------
import           Control.DeepSeq      (NFData (..), deepseq, force)
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.DList           (DList)
import qualified Data.DList           as DList
import           Data.Text
import           GHC.Generics
import Control.DeepSeq.Generics (genericRnf)
----
import           BMake.Lexer
--------------------------------------------------------------------------------

type Parser a = Alex a

thenP :: Parser a -> (a -> Parser b) -> Parser b
thenP = (>>=)

returnP :: a -> Parser a
returnP = return

alexGetPosition :: Alex (AlexPosn)
alexGetPosition = Alex $ \s@AlexState{alex_pos=pos} -> Right (s, pos)

happyError :: Parser a
happyError = do
  (AlexPn _ line col) <- alexGetPosition
  alexStructError (line, col, "syntax error" :: String)

data ExprOneF t
  = Str t
  | Multi (DList (ExprF t))
  | Spaces
  | VarSimple t
  deriving (Show, Generic, Functor)

instance (NFData t) => NFData (ExprOneF t) where
  rnf = genericRnf

type ExprOne = ExprOneF ByteString

type ExprF t = DList (ExprOneF t)
type Expr = ExprF ByteString

data StatementF t
  = Assign t Bool (ExprF t)
  | Local (DList (StatementF t))
  | Target (ExprF t) (ExprF t) (DList (ExprF t))
  | Include t
  | IfCmp Bool (ExprF t) (ExprF t) (DList (StatementF t)) (DList (StatementF t))
  deriving (Show, Generic, Functor)
type Statement = StatementF ByteString

instance (NFData t) => NFData (StatementF t) where
  rnf = genericRnf

data UnitF t
  = Unit {
      unit :: DList (StatementF t)
    } deriving (Show, Generic, Functor)
type Unit = UnitF ByteString

instance (NFData t) => NFData (UnitF t) where
  rnf = genericRnf

instance ToJSON a => ToJSON (DList a) where
    toJSON = fmap toJSON DList.toList

instance ToJSON (ExprOneF Text) where
    toJSON (Str name) = String name
    toJSON (Spaces) = String " "
    toJSON (VarSimple name) =
        object [ "var" .= String name ]
    toJSON (Multi lst) =
        object [ "multi" .= lst ]

instance ToJSON (StatementF Text) where
    toJSON (Assign name isOptional expr) =
        object [ "assign" .= object [
            "name" .= name
          , "isOptional" .= isOptional
          , "expr" .= expr
          ] ]
    toJSON (Local stmts) =
        object [
            "local" .= stmts
          ]
    toJSON (Target outps inps exprs) =
        object [ "target" .= object [
            "outputs" .= outps
          , "inputs" .= inps
          , "recipe" .= exprs
          ] ]
    toJSON (Include name) =
        object [
            "include" .= name
          ]
    toJSON (IfCmp b val_a val_b if_pass if_otherwise) =
        object [ "compare" .= object [
            "is_equal" .= b
          , "val_a" .= val_a
          , "val_b" .= val_b
          , "if_pass" .= if_pass
          , "if_otherwise" .= if_otherwise
          ] ]

instance ToJSON (UnitF Text) where

lexer :: (Token -> Parser a) -> Parser a
lexer f = do
    mPrevTokens <- getPrevToken
    case mPrevTokens of
        (jx, Just token) ->
            setPrevToken (jx, Nothing) >> f token
        (jx, Nothing) -> do
            token <- alexMonadScan
            case token of
                Token _ TokenNewLine ->
                    setPrevToken (Just token, Nothing) >> lexer f
                _ -> case jx of
                            Just prevToken@(Token _ TokenNewLine) -> do
                                case token of
                                    Token _ TokenNewLineAndTab -> do
                                        setPrevToken (Nothing, Nothing)
                                        f token
                                    _ -> do
                                        setPrevToken (Nothing, Just token)
                                        f prevToken
                            _ -> f token