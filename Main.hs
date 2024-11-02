module Main (main) where

import Control.Monad (forever, when)
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hall <- hallMaker
  runGame hall

-- Plan to use a (+1) | (+2) in (mod 3) to form an infallible model
-- Also, consider not storing which door is revealed in a data type at all;
-- instead, consider only ever calculating it on the spot
data Hall = Hall (Maybe (PlayerSelection, Maybe Int)) WinningDoor

type PlayerSelection = Selection

type WinningDoor = Selection

type RevealedSelection = Selection

newtype Selection = Selection Int
  deriving (Eq)

makeSelection :: Integral a => a -> Maybe Selection
makeSelection n
  | n < 0 = Nothing
  | n <= 2 = Just $ Selection $ fromIntegral n
  | otherwise = Nothing

data Item = Car | Goat

instance Show Item where
  show x =
    let go s = "This door contains a " ++ s ++ "."
     in case x of
          Car -> go "car"
          Goat -> go "goat"

hallMaker = undefined

runGame = undefined
