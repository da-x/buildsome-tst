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
  , parseDCToken
  , lexer
  , AssignType(..), IfCmpType(..)
  , MakefileF(..)
  , Makefile
  , StatementF(..), substmts
  , Statement
  , Expr
  , ExprF(..)
  , module BMake.Lexer
  )
  where

--------------------------------------------------------------------------------
import           Control.DeepSeq          (NFData (..))
import           Control.DeepSeq.Generics (genericRnf)
import           Data.Aeson
import           Data.ByteString.Lazy     (ByteString)
import           Data.String              (IsString)
import           Data.Text
import           GHC.Generics
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

data SpecialFlag
  = FirstOutput
  | FirstInput
  | AllInputs
  | AllOOInputs
  deriving (Show, Generic)
instance ToJSON SpecialFlag where
instance NFData SpecialFlag where
  rnf = genericRnf

data SpecialModifier
  = NoMod
  | ModFile
  | ModDir
  deriving (Show, Generic)
instance ToJSON SpecialModifier where

instance NFData SpecialModifier where
  rnf = genericRnf

data ExprF text
  = Str text
  | Multi [[ExprF text]]
  | Spaces
  | VarSpecial SpecialFlag SpecialModifier
  | VarSimple text
  deriving (Show, Generic, Functor)

parseDCToken :: IsString text => (Char, Maybe Char) -> ExprF text
parseDCToken ('.', Nothing) = VarSimple "."
parseDCToken (other, mods) =
    VarSpecial (case other of
                '@' -> FirstOutput
                '<' -> FirstInput
                '^' -> AllInputs
                '|' -> AllOOInputs
                _ -> error $ "unexpected lexing input" ++ show other)
               (case mods of
                 Just 'F' -> ModFile
                 Just 'D' -> ModDir
                 Nothing -> NoMod
                 _ -> error $ "unexpected lexing input" ++ show mods)

instance NFData text => NFData (ExprF text) where
  rnf = genericRnf

type Expr = ExprF ByteString

data AssignType = AssignNormal | AssignConditional
  deriving (Show, Generic)
instance NFData AssignType where
instance ToJSON AssignType where

data IfCmpType = IfEquals | IfNotEquals
  deriving (Show, Generic)
instance NFData IfCmpType where
instance ToJSON IfCmpType where

data StatementF text
  = Assign text AssignType [ExprF text]
  | Local [StatementF text]
  | Target [ExprF text] [ExprF text] [[ExprF text]]
  | Include text
  | IfCmp IfCmpType [ExprF text] [ExprF text] [StatementF text] [StatementF text]
  deriving (Show, Generic, Functor)
type Statement = StatementF ByteString

-- | Traversal of direct children of statement
substmts ::
    Applicative f =>
    ([StatementF text] -> f [StatementF text]) ->
    StatementF text -> f (StatementF text)
substmts f (Local dl) = Local <$> f dl
substmts f (IfCmp a b c dla dlb) = IfCmp a b c <$> f dla <*> f dlb
substmts _ x = pure x

instance NFData text => NFData (StatementF text) where
    rnf = genericRnf

data MakefileF text = Makefile
    { unit :: [StatementF text]
    } deriving (Show, Generic, Functor)
type Makefile = MakefileF ByteString

instance NFData text => NFData (MakefileF text) where

instance ToJSON (ExprF Text) where
    toJSON (Str name) = String name
    toJSON (Spaces) = String " "
    toJSON (VarSpecial vtype mods) =
        object [ "varSpecial" .= vtype
               , "varMod" .= mods ]
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

instance ToJSON (MakefileF Text) where

getPrevTokens :: Alex (Maybe Token, Maybe Token)
getPrevTokens = prevTokens <$> getUserState

modifyPrevTokens :: ((Maybe Token, Maybe Token) -> (Maybe Token, Maybe Token)) -> Alex ()
modifyPrevTokens f = modifyUserState $ \us -> us { prevTokens = f (prevTokens us) }

setPrevTokens :: (Maybe Token, Maybe Token) -> Alex ()
setPrevTokens = modifyPrevTokens . const

lexer :: (Token -> Parser a) -> Parser a
lexer f = do
    mPrevTokens <- getPrevTokens
    case mPrevTokens of
        (jx, Just token) ->
            setPrevTokens (jx, Nothing) >> f token
        (jx, Nothing) -> do
            token <- alexMonadScan
            case token of
                Token _ TokenNewLine ->
                    setPrevTokens (Just token, Nothing) >> lexer f
                _ -> case jx of
                            Just prevToken@(Token _ TokenNewLine) -> do
                                case token of
                                    Token _ TokenNewLineAndTab -> do
                                        setPrevTokens (Nothing, Nothing)
                                        f token
                                    _ -> do
                                        setPrevTokens (Nothing, Just token)
                                        f prevToken
                            _ -> f token
