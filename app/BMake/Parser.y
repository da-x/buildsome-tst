{
{-# LANGUAGE OverloadedStrings #-}
module BMake.Parser where

import BMake.Base
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as Map
import qualified Data.DList as DList
import           Data.DList (DList)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (ByteString)

}

%name happyParser
%tokentype { Token }

%monad { Parser } { thenP } { returnP }
%lexer { lexer } { Token _ TokenEOF }

%token
        include         { Token _ TokenInclude         }
        local           { Token _ TokenLocal           }
        ifeq            { Token _ TokenIfEq            }
        ifneq           { Token _ TokenIfNEq           }
        else            { Token _ TokenElse            }
        endif           { Token _ TokenEndif           }

        OTHER           { Token _ (TokenOther $$)      }
        SPACES          { Token _ (TokenWhitespace $$) }

        "="             { Token _ TokenEqual           }
        "?="            { Token _ TokenEqualMaybe      }
        ":"             { Token _ TokenColon           }
        "("             { Token _ TokenParenOpen       }
        ")"             { Token _ TokenParenClose      }
        "{"             { Token _ TokenCurlyOpen       }
        "}"             { Token _ TokenCurlyClose      }
        ","             { Token _ TokenComma           }
        "%"             { Token _ TokenPercent         }
        "*"             { Token _ TokenAsterik         }
        "$"             { Token _ TokenDollar          }
        DC              { Token _ (TokenDollarChar $$) }
        TAB             { Token _ TokenNewLineAndTab   }
        NEWLINE         { Token _ TokenNewLine         }

%%

-- Regular types

Root  :: {Unit}
       : Statements  { Unit $1 }

Statements :: {[Statement]}
      : StatementsDList { DList.toList $1 }

StatementsDList :: {DList Statement}
      : StatementsDList Statement       { case $2 of { Just x -> $1 `DList.snoc` x ; Nothing -> $1 } }
      | StatementsDList NEWLINE         { $1 }
      | Statement                       { maybe DList.empty DList.singleton $1 }
      |                                 { DList.empty }

-- Maybe whitespace
MW    :: {DList ByteString}
      : SPACES { DList.singleton $1 }
      |        { DList.empty }

Statement :: {Maybe Statement}
      : local MW "{" Statements local MW "}" { Just $ Local $4 }
      | OTHER MW "=" MW TgtExprListE   { Just $ Assign $1 AssignNormal $5 }
      | OTHER MW "?=" MW TgtExprListE  { Just $ Assign $1 AssignConditional $5 }
      | ExprList MW ":" MW TgtExprListE MAYBE_TARGET_BODY
                                      { Just $ Target $1 $5 $6 }
      | include MW OTHER              { Just $ Include $3 }
      | SPACES                        { Nothing }
      | ifeq IFEQ                     { Just $ ($2) $ IfCmp IfEquals }
      | ifneq IFEQ                    { Just $ ($2) $ IfCmp IfNotEquals }

MAYBE_TARGET_BODY :: {DList Expr}
      :                               { DList.empty }
      | TAB SCRIPTS                   { $2 }

SCRIPTS :: {DList Expr}
      : SCRIPTS TAB TgtExprListE      { $1 `DList.snoc` $3 }
      | TgtExprListE                  { DList.singleton $1 }

IFEQ
      : MW "(" ExprListE "," ExprListE ")" NEWLINE Statements else Statements endif
                                      { \x -> x $3 $5 $8 $10 }
      | MW "(" ExprListE "," ExprListE ")" NEWLINE Statements endif
                                      { \x -> x $3 $5 $8 [] }

ExprListE :: {Expr}
      :                               { DList.empty }
      | ExprList                      { $1 }

ExprList :: {Expr}
      : ExprList MW ExprOne           { ($1 `DList.snoc` Spaces) `DList.snoc` $3 }
      | ExprList ExprOne              { $1 `DList.snoc` $2 }
      | ExprOne                       { DList.singleton $1 }

ExprOne :: {ExprOne}
      : OTHER                         { Str $1 }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "{" ExprCommaList "}"         { Multi $2 }
      | SPACES                        { Spaces }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "$"                           { Str "$" }

ExprOneP :: {ExprOne}
      : OTHER                         { Str $1 }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "{" ExprCommaList "}"         { Multi $2 }
      | SPACES                        { Spaces }
      | ","                           { Str "," }
      | "="                           { Str "=" }
      | "?="                          { Str "?=" }
      | ":"                           { Str ":" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "$"                           { Str "$" }
      | "("                           { Str "(" }
      | ")"                           { Str ")" }

TgtExprListE :: {Expr}
      :                               { DList.empty }
      | TgtExprList                   { $1 }

TgtExprList :: {Expr}
      : TgtExprList TgtExprOne        { $1 `DList.snoc` $2 }
      | TgtExprOne                    { DList.singleton $1 }

TgtExprOne :: {ExprOne}
      : OTHER                         { Str $1 }
      | SPACES                        { Spaces }
      | DC                            { parseDCToken $1 }
      | "$" "{" OTHER "}"             { VarSimple $3 }
      | "$"                           { Str "$" }
      | "{" ExprCommaList "}"         { Multi $2 }
      | else                          { Str "else" }
      | ifeq                          { Str "ifeq" }
      | ifneq                         { Str "ifneq" }
      | include                       { Str "include" }
      | local                         { Str "local" }
      | endif                         { Str "endif" }
      | "="                           { Str "=" }
      | ","                           { Str "," }
      | "?="                          { Str "?="}
      | ":"                           { Str ":" }
      | "%"                           { Str "%" }
      | "*"                           { Str "*" }
      | "("                           { Str "(" }
      | ")"                           { Str ")" }
      | "$"                           { Str "$" }

ExprCommaList :: {DList Expr}
      : ExprCommaList "," ExprListNWS { $1 `DList.snoc` $3 }
      | ExprListNWS                   { DList.singleton $1 }

ExprListNWS :: {Expr}
      :                               { DList.empty        }
      | ExprListNWS ExprOneP          { $1 `DList.snoc` $2 }

{}
