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

type Revealed = Bool

data Item = Car | Goat
