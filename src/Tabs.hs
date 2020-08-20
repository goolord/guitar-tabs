{-# LANGUAGE
    LambdaCase
#-}

module Tabs
  ( parseTab
  , decodeTab
  , Tab(..)
  , StringTab(..)
  , Note(..)
  ) where

import Control.Applicative
import Data.Bytes
import Data.Bytes.Parser (Parser)
import Data.Char (toLower, isSpace)
import Data.Word
import qualified Data.Bytes.Parser as P
import qualified Data.Bytes.Parser.Ascii as PA

newtype Tab = Tab [StringTab]

data StringTab = StringTab 
  { baseNote :: Note
  , modNotes :: [Word8]
  }

data Note 
  = A
  | AS
  | B
  | C
  | CS
  | D
  | DS
  | E
  | F
  | FS
  | G
  | GS
  deriving (Eq, Ord, Show, Enum, Bounded)

decodeTab :: Bytes -> Maybe Tab
decodeTab b = P.parseBytesMaybe parseTab b

parseTab :: Parser () s Tab
parseTab = Tab <$>
  many (PA.skipWhile isSpace *> parseStringTab)

parseStringTab :: Parser () s StringTab
parseStringTab = do
  baseNote <- parseNote
  PA.skipChar '|'
  modNotes <- many (sc *> PA.decWord8 ())
  PA.skipChar '|'
  pure $ StringTab baseNote modNotes

parseNote :: Parser () s Note
parseNote = do
  note <- PA.any () >>= \n -> case charToNote n of
    Just x -> pure x
    Nothing -> P.fail ()
  accidental <- PA.peek ()
  case accidental of
    '#' -> P.any () *> (pure $ cycleNext note)
    'b' -> P.any () *> (pure $ cyclePrev note)
    _ -> pure note

sc :: Parser e s ()
sc = PA.skipWhile $ \case
  '-' -> True
  _ -> False

cyclePrev :: (Eq a, Enum a, Bounded a) => a -> a
cyclePrev x = 
  if x == minBound 
  then maxBound 
  else pred x

cycleNext :: (Eq a, Enum a, Bounded a) => a -> a
cycleNext x = 
  if x == maxBound 
  then minBound 
  else succ x

charToNote :: Char -> Maybe Note
charToNote = go . toLower
  where
  go c = case c of
    'a' -> Just A
    'b' -> Just B
    'c' -> Just C
    'd' -> Just D
    'e' -> Just E
    'f' -> Just F
    'g' -> Just G
    _ -> Nothing
