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

data Hall = Hall Selection Door Door Door

newtype Selection = Selection Int
  deriving (Eq)

makeSelection :: Integral a => a -> Maybe Selection
makeSelection n
  | n < 0 = Nothing
  | n <= 2 = Just $ Selection $ fromIntegral n
  | otherwise = Nothing

data Door = Door Revealed Item

instance Show Door where
  show (Door revealed item) =
    case revealed of
      True -> "This door contains a " ++ (show item) ++ "."
      False -> "The contents of this door is unknown."

type Revealed = Bool

data Item = Car | Goat

instance Show Item where
  show Car = "car"
  show Goat = "goat"

instance Show Hall where
  show (Hall (Selection choice) a b c) =
    "You have selected door "
      ++ (show choice)
      ++ ".\n"
      ++ "Door 0: "
      ++ (show a)
      ++ "\n"
      ++ "Door 1: "
      ++ (show b)
      ++ "\n"
      ++ "Door 2: "
      ++ (show c)

hallMaker = undefined

runGame = undefined
