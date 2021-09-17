module Core where

data Movement = PushUp | PullUp | WheelHouse | Abs | Assist | Grip | Neck deriving (Eq, Show)

data Gear = Kettlebell | Dumbbell deriving (Eq, Show)

data Exercise = Exercise
  { name :: String
  , movement :: Movement
  , gear :: Maybe Gear
  } deriving (Eq, Show)

newtype Reps = Reps Int

data ExerciseSet = ExerciseSet
  { exeExercise :: Exercise
  , reps :: Reps
  }
