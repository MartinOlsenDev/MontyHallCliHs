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

data Phase3 doorId = Phase3 doorId Winning RevealedDoorNext WasWinning

newtype DoorId = DoorId Integer
  deriving (Show, Eq, Integral, Real, Ord, Num, Enum)

type WasWinning = Bool

type Winning = Bool

type MadeSwitch = Bool

type RevealedDoorNext = Bool

type PlayerSelection = DoorId

type WinningDoor = DoorId

type RevealedSelection = DoorId

createPhase1 :: IO (Phase1 DoorId)
createPhase1 = do
  randomDoor <- curry randomRIO 0 2
  return $ Phase1 $ DoorId randomDoor

-- advance, selectedDoor, and revealedDoor should all be within
-- one or more typeclasses
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
advance2 hall@(Phase2 winDoor isWinning isNext) switched =
  let nowWinning = logicalXor isWinning switched
   in Phase3 winDoor nowWinning isNext isWinning

revealedDoor :: Integral a => Phase2 a -> a
revealedDoor (Phase2 winDoor isWinning isNext) =
  fromIntegral $
    if isNext
      then (winDoor + 1) `mod` 3
      else (winDoor + 2) `mod` 3

selectedDoor :: Integral a => Phase2 a -> a
selectedDoor hall@(Phase2 winDoor isWinning isNext) =
  if isWinning
    then winDoor
    else fromJust $ newFinder [winDoor, revealedDoor hall] $ enumFromTo 0 2

instance Show (Phase1 a) where
  show _ = "The hall has 3 closed doors."

instance (Integral a, Show a) => Show (Phase2 a) where
  show hall =
    "You have selected door " ++ show (selectedDoor hall) ++ ".\nThe revealed door is " ++ show (revealedDoor hall) ++ ".\nIt has been revealed to be a goat."

prevLoc :: Integral a => Phase3 a -> a
prevLoc (Phase3 winLoc isWinning isNext wasWinning) =
  if wasWinning
    then winLoc
    else selectedDoor (Phase2 winLoc isWinning isNext)

instance (Integral a, Show a) => Show (Phase3 a) where
  show hall@(Phase3 winLoc isWinning isNext wasWinning) =
    let madeSwitch = not $ (isWinning && wasWinning) || (not isWinning && not wasWinning)
        currentLoc = if isWinning then winLoc else selectedDoor (Phase2 winLoc isWinning isNext)
        switchRemark =
          ( if madeSwitch
              then "You switched from door " ++ show (prevLoc hall) ++ " to door " ++ show currentLoc
              else "You stayed at door " ++ show (prevLoc hall)
          )
            ++ ".\n"
        winRemark = if isWinning then "You won!\n" else "You lost.\n"
     in switchRemark
          ++ "The winning door is door "
          ++ show winLoc
          ++ winRemark

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
