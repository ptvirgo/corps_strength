module Core where

data Movement = PushUp | PullUp | WheelHouse | Abs | Assist | Grip | Neck deriving (Eq, Show)

data Gear = Kettlebell | Dumbbell deriving (Eq, Show)

data Exercise = Exercise
  { name :: String
  , movement :: Movement
  , gear :: Maybe Gear
  } deriving (Eq, Show)

type Reps = Int

data ExerciseSet = ExerciseSet
  { exercise :: Exercise
  , reps :: Reps
  } deriving (Eq)

instance Show ExerciseSet where
  show x = (name . exercise $ x) ++ " x " ++ (show . reps $ x) ++ gstring where
    gstring =
      case gear . exercise $ x of
        Just g -> " (" ++ show g ++ ")"
        Nothing -> ""
