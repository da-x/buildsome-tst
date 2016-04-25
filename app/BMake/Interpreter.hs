{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module BMake.Interpreter
    ( interpret
    ) where

import           BMake.Base
import           BMake.Data
import           Control.DeepSeq (NFData(..), force)
import           Control.Exception (evaluate)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Reader (ReaderT(..))
import qualified Control.Monad.Trans.Reader as Reader
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS8
import           Data.Function ((&))
import           Data.IORef
import           Data.List (intercalate)
import           Data.List.Split (splitOn)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)

import qualified Lib.Makefile.Types as MT

type Vars = Map ByteString [Expr]

data Env = Env
    { envVars :: IORef Vars
    , envTargets :: IORef [MT.Target]
    , envPatterns :: IORef [MT.Pattern]
    }

type M = ReaderT Env IO

run :: MT.Vars -> M a -> IO a
run vars act =
    do
        let x = Map.fromList $ map (\(k, v) ->
              (BS8.fromChunks [k], [Str $ BS8.fromChunks [v]])) $ Map.toList vars
        varsRef <- newIORef x
        a <- newIORef []
        b <- newIORef []
        runReaderT act Env
            { envVars = varsRef
            , envTargets = a
            , envPatterns = b
            }

interpret :: Makefile -> MT.Vars -> IO MT.Makefile
interpret bmakefile vars = (run vars) . makefile $ bmakefile

-- | Expr after variable substitution
data Expr1
  = Expr1'Str ByteString
  | Expr1'OpenBrace
  | Expr1'CloseBrace
  | Expr1'Comma
  | Expr1'Spaces
  | Expr1'VarSpecial MetaVar MetaVarModifier
  deriving (Eq, Ord, Show, Generic)
instance NFData Expr1

-- | Expr1 after Group parsing
data Expr2
  = Expr2'Str ByteString
  | Expr2'Group [[Expr2]]
  | Expr2'Spaces
  | Expr2'VarSpecial MetaVar MetaVarModifier
  deriving (Eq, Ord, Show, Generic)
instance NFData Expr2


{- TODO:
data ExprTopLevel
  = ExprTopLevel'Str ByteString
  | ExprTopLevel'Spaces
-}

makefile :: Makefile -> M MT.Makefile
makefile (Makefile stmts) =
    do statements stmts

       Env{..} <- Reader.ask
       a <- liftIO $ readIORef envTargets
       b <- liftIO $ readIORef envPatterns

       return $ MT.Makefile {
            MT.makefileTargets = a
          , MT.makefilePatterns = b
          , MT.makefilePhonies = [] -- TODO
          , MT.makefileWeakVars = Map.fromList [] -- TODO
          }

-- TODO: Take the data ctors so it can generate an ExprTopLevel
subst :: Vars -> [Expr] -> [Expr1]
subst vars =
    concatMap go
    where
        go OpenBrace = [Expr1'OpenBrace]
        go CloseBrace = [Expr1'CloseBrace]
        go Comma = [Expr1'Comma]
        go (Str text) = [Expr1'Str text]
        go Spaces = [Expr1'Spaces]
        go (VarSpecial flag modifier) =
            [Expr1'VarSpecial flag modifier]
        go (VarSimple var) =
            concatMap go val
            where
                val =
                    Map.lookup var vars
                    & fromMaybe
                    (error ("Invalid var-ref: " ++ show var))

{-# INLINE first #-}
first :: (a -> a') -> (a, b) -> (a', b)
first f (x, y) = (f x, y)

parseGroups :: [Expr1] -> [Expr2]
parseGroups = \case
    Expr1'Str str:xs -> Expr2'Str str : parseGroups xs
    Expr1'OpenBrace:xs ->
        let (groups, ys) = commaSplit xs
        in Expr2'Group groups : parseGroups ys
    Expr1'CloseBrace:_ -> error "Close brace without open brace!"
    Expr1'Comma:xs -> Expr2'Str "," : parseGroups xs
    Expr1'Spaces:xs -> Expr2'Spaces : parseGroups xs
    Expr1'VarSpecial mv m:xs -> Expr2'VarSpecial mv m : parseGroups xs
    [] -> []
    where
        prependFirstGroup :: Expr2 -> [[Expr2]] -> [[Expr2]]
        prependFirstGroup _ [] = error "Result with no options!"
        prependFirstGroup x (alt:alts) = (x:alt) : alts
        prepend = first . prependFirstGroup
        commaSplit :: [Expr1] -> ([[Expr2]], [Expr1])
        commaSplit = \case
            Expr1'CloseBrace:xs -> ([[]], xs)
            Expr1'OpenBrace:xs ->
                let (childGroups, ys) = commaSplit xs
                in commaSplit ys & prepend (Expr2'Group childGroups)
            Expr1'Comma:xs -> commaSplit xs & first ([] :)
            Expr1'Str str:xs ->
                commaSplit xs & prepend (Expr2'Str str)
            Expr1'Spaces:xs -> commaSplit xs & prepend Expr2'Spaces
            Expr1'VarSpecial mv m:xs -> commaSplit xs & prepend (Expr2'VarSpecial mv m)
            [] -> error "Missing close brace!"

data CompressDetails = WithSpace | WithoutSpace
    deriving Eq

compress :: CompressDetails -> [Expr3] -> [Expr3]
compress _ [] = []
compress cd xs' =
    case span spaceOrStr xs' of
        ([], (x:xs)) -> x:compress cd xs
        ([x], xs)    -> x:compress cd xs
        (ys, xs)     -> (Expr3'Str $ BS8.concat $ map toStr ys):compress cd xs

  where
    ws = cd == WithSpace

    spaceOrStr Expr3'Spaces  = ws
    spaceOrStr (Expr3'Str _) = True
    spaceOrStr _             = False

    toStr Expr3'Spaces       = " "
    toStr (Expr3'Str s)      = s
    toStr _                  = ""

cartesian :: [Expr2] -> [Expr3]
cartesian input = afterExpand
--    (\output -> trace ("cartesian input: " ++ show input ++ "\n   output: " ++ show output) output) .
    where
        afterExpand = unwords2 . map unwords2 . map go . splitOn [Expr2'Spaces] $ input
        unwords2 = intercalate [Expr3'Spaces]
        go :: [Expr2] -> [[Expr3]]
        go (Expr2'Spaces:_) = error "splitOn should leave no Spaces!"
        go (Expr2'Str str:xs) = (Expr3'Str str:) <$> go xs
        go (Expr2'VarSpecial mv m:xs) = (Expr3'VarSpecial mv m:) <$> go xs
        go (Expr2'Group options:xs) =
            (++) <$> concatMap go options <*> go xs
        go [] = [[]]

normalize :: Vars -> [Expr] -> [Expr3]
normalize vars = cartesian . parseGroups . subst vars

assign :: ByteString -> AssignType -> [Expr] -> M ()
assign name assignType exprL =
    do
        varsRef <- Reader.asks envVars
        let f AssignConditional (Just old) = Just old
            f _ _ = Just exprL
        Map.alter (f assignType) name
            & modifyIORef' varsRef
            & liftIO

local :: [Statement] -> M ()
local stmts =
    do
        varsRef <- Reader.asks envVars
        varsSnapshot <- readIORef varsRef & liftIO
        res <- statements stmts
        writeIORef varsRef varsSnapshot & liftIO
        return res

showExprL :: [Expr3] -> String
showExprL = concatMap showExpr

showExpr :: Expr3 -> String
showExpr (Expr3'Str text) = BS8.unpack text
showExpr Expr3'Spaces = " "
showExpr (Expr3'VarSpecial specialFlag specialModifier) =
    "$" ++ wrap flagChar
    where
        flagChar =
            case specialFlag of
            FirstOutput -> "@"
            FirstInput -> "<"
            AllInputs -> "^"
            AllOOInputs -> "|"
            Stem -> "*"
        wrap =
            case specialModifier of
            NoMod -> id
            ModFile -> ('(':) . (++"F)")
            ModDir -> ('(':) . (++"D)")

statements :: [Statement] -> M ()
statements = mapM_ statement

target :: [Expr] -> [Expr] -> [[Expr]] -> M ()
target outputs inputs {-orderOnly-} script =
    do
        vars <- Reader.asks envVars >>= liftIO . readIORef
        let norm = normalize vars
        outs  <- liftIO $ evaluate $ force $ compress WithoutSpace $ norm outputs
        ins   <- liftIO $ evaluate $ force $ compress WithoutSpace $ norm inputs
        scrps <- liftIO $ evaluate $ force $ map (compress WithSpace . norm) script

        let put = liftIO . putStrLn
        put "target:"
        put $ "     outs: " ++ showExprL outs
        put $ "     ins:  " ++ showExprL ins
        put $ "     script:"
        put $ show ins
        put $ show outs
        put $ show scrps
        mapM_ (put . ("        "++) . showExprL) scrps
        return ()

-- Expanded exprs given!
-- TODO: Consider type-level marking of "expanded" exprs
cmp :: IfCmpType -> [Expr3] -> [Expr3] -> Bool
cmp IfEquals = (==)
cmp IfNotEquals = (/=)

ifCmp ::
    IfCmpType -> [Expr] -> [Expr] ->
    [Statement] -> [Statement] -> M ()
ifCmp ifCmpType exprA exprB thenStmts elseStmts =
    do
        vars <- Reader.asks envVars >>= liftIO . readIORef
        let norm = normalize vars
        statements $
            if cmp ifCmpType (norm exprA) (norm exprB)
            then thenStmts
            else elseStmts

statement :: Statement -> M ()
statement =
    \case
    Assign name assignType exprL -> assign name assignType exprL
    Local stmts -> local stmts
    Target outputs inputs script -> target outputs inputs script
    IfCmp cmpType exprA exprB thenStmts elseStmts ->
        ifCmp cmpType exprA exprB thenStmts elseStmts
    Include {} -> error "Include should have been expanded"
