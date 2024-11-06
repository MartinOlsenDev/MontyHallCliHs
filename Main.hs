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
  hall <- createPhase1
  runGame hall
  return ()

data Phase1 = Phase1 DoorId

data Phase2 = Phase2 DoorId Winning RevealedDoorNext

data Phase3 = Phase3 DoorId Winning RevealedDoorNext WasWinning

newtype DoorId = DoorId Integer
  deriving (Show, Eq, Integral, Real, Ord, Num, Enum)

makeDoorId :: String -> DoorId
makeDoorId s = DoorId (read s :: Integer)

type WasWinning = Bool

type Winning = Bool

type RevealedDoorNext = Bool

type PlayerSelection = DoorId

createPhase1 :: IO Phase1
createPhase1 = do
  randomDoor <- curry randomRIO 0 2
  return $ Phase1 $ DoorId randomDoor

-- selectedDoor and revealedDoor should all be within
-- one or more typeclasses
advance1 :: Phase1 -> DoorId -> IO Phase2
advance1 (Phase1 winDoor) choice = do
  if winDoor == choice
    then do
      randomDoor <- curry randomRIO 0 1
      return $ Phase2 winDoor True (randomDoor == (1 :: Int))
    else
      let unusedDoor = fromJust . newFinder [winDoor, choice] $ enumFromTo 0 2
          isNext = (winDoor + 1) `mod` 3 == unusedDoor
       in return $ Phase2 winDoor False isNext

advance2 :: Phase2 -> Bool -> Phase3
advance2 hall@(Phase2 winDoor isWinning isNext) switched =
  let nowWinning = logicalXor isWinning switched
   in Phase3 winDoor nowWinning isNext isWinning

class SelectedStage a where
  selectedDoor :: a -> DoorId
  revealedDoor :: a -> DoorId

instance SelectedStage Phase2 where
  revealedDoor (Phase2 winDoor isWinning isNext) =
    fromIntegral $
      if isNext
        then (winDoor + 1) `mod` 3
        else (winDoor + 2) `mod` 3
  selectedDoor hall@(Phase2 winDoor isWinning isNext) =
    if isWinning
      then winDoor
      else fromJust $ newFinder [winDoor, revealedDoor hall] $ enumFromTo 0 2

instance SelectedStage Phase3 where
  revealedDoor (Phase3 winDoor isWinning isNext _) =
    fromIntegral $
      if isNext
        then (winDoor + 1) `mod` 3
        else (winDoor + 2) `mod` 3
  selectedDoor hall@(Phase3 winDoor isWinning isNext _) =
    if isWinning
      then winDoor
      else fromJust $ newFinder [winDoor, revealedDoor hall] $ enumFromTo 0 2

instance Show Phase1 where
  show _ = "The hall has 3 closed doors."

instance Show Phase2 where
  show hall =
    "You have selected door " ++ show (selectedDoor hall) ++ ".\nThe revealed door is " ++ show (revealedDoor hall) ++ ". It has been revealed to be a goat."

prevLoc :: Phase3 -> DoorId
prevLoc (Phase3 winLoc _ isNext wasWinning) = selectedDoor (Phase2 winLoc wasWinning isNext)

instance Show Phase3 where
  show hall@(Phase3 winLoc isWinning isNext wasWinning) =
    let madeSwitch = not $ (isWinning && wasWinning) || (not isWinning && not wasWinning)
        currentLoc = if isWinning then winLoc else selectedDoor hall
        switchRemark =
          ( if madeSwitch
              then "You switched from door " ++ show (prevLoc hall) ++ " to door " ++ show currentLoc
              else "You stayed at door " ++ show (prevLoc hall)
          )
            ++ ".\n"
        winRemark = if isWinning then "You won!" else "You lost."
     in switchRemark
          ++ "The winning door is door "
          ++ show winLoc
          ++ ". "
          ++ winRemark

-- given two lists, find the only member of the second not
-- occuring in the first
newFinder :: Eq a => [a] -> [a] -> Maybe a
newFinder a = find (\x -> notElem x a)

logicalXor :: Bool -> Bool -> Bool
logicalXor a b = (a || b) && not (a && b)

runGame :: Phase1 -> IO ()
runGame game = do
  putStrLn $ "You look at the hall.\n" ++ show game ++ "\nWhat door do you choose?"
  choice <- getLine
  p2 <- advance1 game ((makeDoorId choice) :: DoorId)
  putStrLn $ "\nYou look at the hall.\n" ++ show p2 ++ "\nDo you choose to switch?\ny for yes, anything else for no."
  switchChoice <- getLine
  let stageOutcome = advance2 p2 (switchChoice == "y")
  putStrLn $ "\n" ++ show stageOutcome
  return ()
