{-# LANGUAGE DeriveGeneric #-}
module BMake.User
       ( parseWithAlex
       , parseUnit
       , stateBase
       , Error(..)
       ) where

--------------------------------------------------------------------------------
import           Control.DeepSeq (NFData)
import qualified Data.ByteString.Lazy as BL
import qualified Data.DList as DList
import           Data.DList (DList)
import           GHC.Generics (Generic)
----
import           BMake.Parser (happyParser)
import           BMake.Lexer
import           BMake.Base
--------------------------------------------------------------------------------

data Error = Error !Int !Int !String
   deriving (Show, Generic)

instance NFData Error

stateBase :: Int
stateBase = 0

parseWithAlex :: Int -> BL.ByteString -> Either String (DList Token)
parseWithAlex startState bs = root
    where root = runAlex bs (alexSetStartCode startState >> loop DList.empty)
          loop s' = do
              lexer $ \token@(Token _ cls) -> do
                  case cls of
                      TokenEOF -> return s'
                      _ -> loop $ s' `DList.snoc` token


parseUnit :: BL.ByteString -> Either Error Unit
parseUnit s =
  case runAlex s $ happyParser of
    Right x -> Right x
    Left ('l':'e':'x':xs) ->
      Left (Error 0 0 xs) -- TODO
    Left ('s':'h':'o':'w':'-':'e':'r':'r':'o':'r':':':' ':xs) ->
      let (line, column, e) = (read xs :: (Int, Int, String))
       in Left (Error line column (e :: String))
    Left xs -> Left (Error 0 0 xs)

