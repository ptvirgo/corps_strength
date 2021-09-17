module Exercises where

import Data.List.NonEmpty (NonEmpty (..))
import Core

glossary :: NonEmpty Exercise
glossary =
  Exercise "Regular Pull-Up" PullUp Nothing
  :|
  [ Exercise "Behind the Neck Pull-Up" PullUp Nothing
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
  , Exercise "Curls" Assist $ Just Kettlebell
  , Exercise "Curls" Assist $ Just Dumbbell
  , Exercise "Hammer Curls" Assist $ Just Kettlebell
  , Exercise "Hammer Curls" Assist $ Just Dumbbell
  , Exercise "Upright Row" Assist $ Just Kettlebell
  , Exercise "Upright Row" Assist $ Just Dumbbell
  , Exercise "Front Raise" Assist $ Just Kettlebell
  , Exercise "Front Raise" Assist $ Just Dumbbell
  , Exercise "Triceps Press" Assist $ Just Kettlebell
  , Exercise "Triceps Press" Assist $ Just Dumbbell
  ]
