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
  , AssignType(..)
  , IfCmpType(..)
  , MetaVar(..)
  , MetaVarModifier(..)
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

data MetaVar
  = FirstOutput
  | FirstInput
  | AllInputs
  | AllOOInputs
  | Stem
  deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVar where
instance NFData MetaVar where
  rnf = genericRnf

data MetaVarModifier
  = NoMod
  | ModFile
  | ModDir
  deriving (Eq, Ord, Show, Generic)
instance ToJSON MetaVarModifier where
instance NFData MetaVarModifier where
  rnf = genericRnf

data ExprF text
  = Str text
  | OpenBrace
  | CloseBrace
  | Comma
  | Spaces
  | VarSpecial MetaVar MetaVarModifier
  | VarSimple text
  deriving (Eq, Ord, Show, Generic, Functor)

type Expr = ExprF ByteString

parseMetaVarChar :: Char -> MetaVar
parseMetaVarChar '@' = FirstOutput
parseMetaVarChar '<' = FirstInput
parseMetaVarChar '^' = AllInputs
parseMetaVarChar '|' = AllOOInputs
parseMetaVarChar '*' = Stem
parseMetaVarChar other = error $ "unknown meta-variable: $" ++ [other]

parseModifier :: Maybe Char -> MetaVarModifier
parseModifier Nothing = NoMod
parseModifier (Just 'F') = ModFile
parseModifier (Just 'D') = ModDir
parseModifier (Just other) = error $ "unknown meta-variable modifier: $(," ++ [other] ++ ")"

parseDCToken :: IsString text => (Char, Maybe Char) -> ExprF text
parseDCToken ('.', Nothing) = VarSimple "."
parseDCToken (other, modifier) = VarSpecial (parseMetaVarChar other) (parseModifier modifier)

instance NFData text => NFData (ExprF text) where
  rnf = genericRnf

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
    toJSON Comma = String ","
    toJSON OpenBrace = String "{"
    toJSON CloseBrace = String "}"
    toJSON (Spaces) = String " "
    toJSON (VarSpecial vtype mods) =
        object [ "varSpecial" .= vtype
               , "varMod" .= mods ]
    toJSON (VarSimple name) =
        object [ "var" .= String name ]

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
