{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Control.Monad (forever, when)
import Data.List (find)
import Data.Maybe
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import System.Random (randomRIO)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hall <- hallMaker
  runGame hall

data Phase1 doorId = Phase1 doorId

data Phase2 doorId = Phase2 doorId Winning RevealedDoorNext

data Phase3 doorId = Phase3 doorId Winning doorId

newtype DoorId = DoorId Integer
  deriving (Eq, Integral, Real, Ord, Num, Enum)

type Winning = Bool

type RevealedDoorNext = Bool

type PlayerSelection = DoorId

type WinningDoor = DoorId

type RevealedSelection = DoorId

createPhase1 :: IO (Phase1 DoorId)
createPhase1 = do
  randomDoor <- curry randomRIO 0 2
  return $ Phase1 $ DoorId randomDoor

advance1 :: Integral a => Phase1 a -> a -> IO (Phase2 a)
advance1 (Phase1 winDoor) choice = do
  if winDoor == choice
    then do
      randomDoor <- curry randomRIO 0 1
      return $ Phase2 winDoor True (randomDoor == (1 :: Int))
    else
      let unusedDoor = fromJust . newFinder [winDoor, choice] $ enumFromTo 0 2
          isNext = (winDoor + 1) `mod` 3 == unusedDoor
       in return $ Phase2 winDoor False isNext

advance2 :: Integral a => Phase2 a -> Bool -> Phase3 a
advance2 (Phase2 winDoor isWinning isNext) switched =
  let nowWinning = logicalXor isWinning switched
      revealedDoor =
        fromIntegral $
          if isNext
            then (winDoor + 1) `mod` 3
            else (winDoor + 2) `mod` 3
      prevLoc =
        if isWinning
          then winDoor
          else fromJust $ newFinder [winDoor, revealedDoor] $ enumFromTo 0 2
   in Phase3 winDoor nowWinning prevLoc

data Item = Car | Goat

instance Show Item where
  show x =
    let go s = "This door contains a " ++ s ++ "."
     in case x of
          Car -> go "car"
          Goat -> go "goat"

-- given two lists, find the only member of the second not
-- occuring in the first
newFinder :: Eq a => [a] -> [a] -> Maybe a
newFinder a = find (\x -> notElem x a)

logicalXor :: Bool -> Bool -> Bool
logicalXor a b = (a || b) && not (a && b)

makeSelection :: Integral a => a -> Maybe DoorId
makeSelection n
  | n < 0 = Nothing
  | n <= 2 = Just $ DoorId $ fromIntegral n
  | otherwise = Nothing

hallMaker = undefined

runGame = undefined
