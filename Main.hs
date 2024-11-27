{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (main) where

import Data.List (find)
import Data.Maybe
import System.IO
  ( BufferMode (NoBuffering),
    hSetBuffering,
    stdout,
  )
import System.Random (randomRIO)
import Text.Read (readMaybe)

main :: IO ()
main =
  hSetBuffering stdout NoBuffering
    >> createPhase1
    >>= runGame

runGame :: Phase1 -> IO ()
runGame game =
  putStrLn (concat ["You look at the hall.\n", show game, "\n"])
    >> phase1Choice
    >>= advance1 game
    >>= ( \phase2 ->
            putStrLn
              ( concat
                  [ "\nYou look at the hall.\n",
                    show phase2,
                    "\nDo you choose to switch?\ny for yes, anything else for no."
                  ]
              )
              >> getLine
              >>= (putStrLn . ("\n" ++) . show) . advance2 phase2 . (== "y")
        )

type WasWinning = Bool

type Winning = Bool

type RevealedDoorNext = Bool

type PlayerSelection = DoorId

data Phase1 = Phase1 DoorId

data Phase2 = Phase2 DoorId Winning RevealedDoorNext

data Phase3 = Phase3 DoorId Winning RevealedDoorNext WasWinning

newtype DoorId = DoorId Integer
  deriving (Show, Eq, Integral, Real, Ord, Num, Enum)

-- Replace this with a monadic untilM-like
-- implementation at some point
phase1Choice :: IO DoorId
phase1Choice =
  let validInt :: Integer -> Bool
      validInt = flip elem [0, 1, 2]

      phase1Choice' :: Maybe Integer -> IO Integer
      phase1Choice' (Just x) =
        if validInt x
          then return x
          else phase1Choice' Nothing
      phase1Choice' Nothing = putStrLn "That was not a valid door ID. Choose 0, 1, or 2." >> readMaybe <$> getLine >>= phase1Choice'
   in putStrLn "What door do you choose?" >> DoorId <$> (getLine >>= phase1Choice' . readMaybe)

createPhase1 :: IO Phase1
createPhase1 = Phase1 . DoorId <$> randomRIO (0, 2)

advance1 :: Phase1 -> DoorId -> IO Phase2
advance1 (Phase1 winDoor) choice =
  if winDoor == choice
    then Phase2 winDoor True <$> ((== (1 :: Int)) <$> randomRIO (0, 1))
    else
      let unusedDoor = fromJust $ newFinder [winDoor, choice] $ enumFromTo 0 2
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
  revealedDoor (Phase3 winDoor _ isNext _) =
    fromIntegral $
      if isNext
        then (winDoor + 1) `mod` 3
        else (winDoor + 2) `mod` 3
  selectedDoor hall@(Phase3 winDoor isWinning _ _) =
    if isWinning
      then winDoor
      else fromJust $ newFinder [winDoor, revealedDoor hall] $ enumFromTo 0 2

prevLoc :: Phase3 -> DoorId
prevLoc (Phase3 winLoc _ isNext wasWinning) = selectedDoor (Phase2 winLoc wasWinning isNext)

instance Show Phase1 where
  show _ = "The hall has 3 closed doors."

instance Show Phase2 where
  show hall =
    concat
      [ "You have selected door ",
        show (selectedDoor hall),
        ".\nThe revealed door is ",
        show (revealedDoor hall),
        ". It has been revealed to be a goat."
      ]

instance Show Phase3 where
  show hall@(Phase3 winLoc isWinning isNext wasWinning) =
    let madeSwitch = not $ (isWinning && wasWinning) || (not isWinning && not wasWinning)
        currentLoc = if isWinning then winLoc else selectedDoor hall
        switchRemark =
          ( if madeSwitch
              then
                concat
                  [ "You switched from door ",
                    show (prevLoc hall),
                    " to door ",
                    show currentLoc
                  ]
              else "You stayed at door " ++ show (prevLoc hall)
          )
            ++ ".\n"
        winRemark = if isWinning then "You won!" else "You lost."
     in concat
          [ switchRemark,
            "The winning door is door ",
            show winLoc,
            ". ",
            winRemark
          ]

-- given two lists, find first member of the second not
-- occuring in the first
newFinder :: Eq a => [a] -> [a] -> Maybe a
newFinder a = find (\x -> notElem x a)

logicalXor :: Bool -> Bool -> Bool
logicalXor a b = (a || b) && not (a && b)
