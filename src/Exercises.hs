module Exercises where

-- import Data.List.NonEmpty (NonEmpty(..))
import Control.Monad (replicateM)
import System.Random (randomRIO)
import Core

glossary :: [Exercise]
glossary =
  [ Exercise "Regular Pull-Up" PullUp Nothing
  , Exercise "Behind the Neck Pull-Up" PullUp Nothing
  , Exercise "Chin-Up" PullUp Nothing
  , Exercise "Close-Grip Pull-Up" PullUp Nothing
  , Exercise "Commando Pull-Up" PullUp Nothing
  , Exercise "V-Up" PullUp Nothing
  , Exercise "Regular Push-Up" PushUp Nothing
  , Exercise "Wide Push-Up" PushUp Nothing
  , Exercise "Incline Push-Up" PushUp Nothing
  , Exercise "Diamond Push-Up" PushUp Nothing
  , Exercise "Mountain Climber Push-Up" PushUp Nothing
  , Exercise "Deadlift Curl" WheelHouse $ Just Kettlebell
  , Exercise "Deadlift Curl" WheelHouse $ Just Dumbbell
  , Exercise "Squat Press" WheelHouse $ Just Kettlebell
  , Exercise "Squat Press" WheelHouse $ Just Dumbbell
  , Exercise "Step-Up Shrug" WheelHouse $ Just Kettlebell
  , Exercise "Step-Up Shrug" WheelHouse $ Just Dumbbell
  , Exercise "Lateral Swing" WheelHouse $ Just Kettlebell
  , Exercise "Lateral Swing" WheelHouse $ Just Dumbbell
  , Exercise "Rowing" WheelHouse $ Just Kettlebell
  , Exercise "Rowing" WheelHouse $ Just Dumbbell
  , Exercise "Regular Crunch" Abs Nothing
  , Exercise "Flutter Kicks" Abs Nothing
  , Exercise "Elevated Crunch" Abs Nothing
  , Exercise "Hello Dollies" Abs Nothing
  , Exercise "Side Crunches" Abs Nothing
  , Exercise "Leg Lifts" Abs Nothing
  , Exercise "Reach Crunches" Abs Nothing
  , Exercise "Curls" Assist $ Just Kettlebell
  , Exercise "Curls" Assist $ Just Dumbbell
  , Exercise "Hammer Curl" Assist $ Just Kettlebell
  , Exercise "Hammer Curl" Assist $ Just Dumbbell
  , Exercise "Upright Row" Assist $ Just Kettlebell
  , Exercise "Upright Row" Assist $ Just Dumbbell
  , Exercise "Front Raise" Assist $ Just Kettlebell
  , Exercise "Front Raise" Assist $ Just Dumbbell
  , Exercise "Triceps Press" Assist $ Just Kettlebell
  , Exercise "Triceps Press" Assist $ Just Dumbbell
  , Exercise "8 Count" Assist Nothing
  , Exercise "Free Squat" Assist Nothing
  , Exercise "Step-Up" Assist Nothing
  , Exercise "Mountain Climber" Assist Nothing
  , Exercise "Wrist Curl" Grip $ Just Dumbbell
  , Exercise "Wrist Curl" Grip $ Just Kettlebell
  , Exercise "Neck Bridge" Neck Nothing
  , Exercise "Neck Curl" Neck $ Just Kettlebell
  , Exercise "Neck Curl" Neck $ Just Dumbbell
  ]

pickExercise :: (Exercise -> Bool) -> IO Exercise
pickExercise f = do
  let opts = filter f glossary
      count = length opts

  n <- randomRIO (0, count - 1)

  return $
    if count < 1
       then Exercise "Infinite Headstand" Assist Nothing
       else opts !! n

makeGearedMission :: Gear -> IO [ExerciseSet]
makeGearedMission g = do
  pullups <- pickExercise (\x -> movement x == PullUp)
  pushups <- pickExercise (\x -> movement x == PushUp)
  wheelhouse <- pickExercise (\x -> movement x == WheelHouse && gear x == Just g)
  assist <- pickExercise (\x -> movement x == Assist && gear x == Just g)
  gripOrNeck <- pickExercise (\x -> elem (movement x) [Grip, Neck] && elem (gear x) [Nothing, Just g])
  abs <- replicateM 3 $ pickExercise (\x -> movement x == Abs && elem (gear x) [Nothing, Just g])

  return $
    (take 6 $ cycle [ ExerciseSet pullups 10, ExerciseSet pushups 25 ])
    ++ (take 6 $ cycle [ ExerciseSet wheelhouse 15, ExerciseSet (abs !! 0) 50 ])
    ++ (take 6 $ cycle [ ExerciseSet assist 15, ExerciseSet (abs !! 1) 50 ])
    ++ [ ExerciseSet gripOrNeck 3, ExerciseSet (abs !! 2) 50 ]

makeGearlessMission :: IO [ExerciseSet]
makeGearlessMission = do
  pullups <- pickExercise (\x -> movement x == PullUp)
  pushups <- pickExercise (\x -> movement x == PushUp)
  assists <- replicateM 2 $ pickExercise (\x -> movement x == Assist && gear x == Nothing)
  abs <- replicateM 2 $ pickExercise (\x -> movement x == Abs && gear x == Nothing)

  return $
    (take 9 $ cycle [ ExerciseSet pullups 10, ExerciseSet pushups 25, ExerciseSet (abs !! 0) 50 ])
    ++ (take 9 $ cycle [ ExerciseSet (assists !! 0) 25, ExerciseSet (assists !! 1) 25, ExerciseSet (abs !! 1) 50 ])
