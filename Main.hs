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

data Hall = Hall (Maybe Selection) (Maybe Selection) Item Item Item

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

type Revealed = Bool

hallMaker = undefined

runGame = undefined
