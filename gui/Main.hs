{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.Async ( async )
import Control.Monad ( void )

import Data.ByteString ( ByteString )
import Data.Text as Text

import GI.Gtk ( Align (..)
              , Box (..)
              , Button (..)
              , Orientation (..)
              , Label (..)
              , Window (..)
              )

import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import Core
import Exercises

{- App State -}

data State = GearPage | WorkoutPage [ ExerciseSet ]

initialstate' :: State
initialstate' = GearPage

{- Updates -}

data Event = ChoseGear ( Maybe Gear ) | SetWorkout [ ExerciseSet ] | Quit

update' :: State -> Event -> Transition State Event
update' s ( ChoseGear mg ) = Transition s $ loadWorkout mg
update' _ ( SetWorkout exercises ) = Transition ( WorkoutPage exercises ) ( return Nothing )
update' _ Quit = Exit

loadWorkout :: Maybe Gear -> IO ( Maybe Event )
loadWorkout mg = do
  exerciseSet <-
      case mg of
           Nothing -> makeGearlessMission
           Just g -> makeGearedMission g
  return . Just . SetWorkout $ exerciseSet

fewerReps :: Int -> Int
fewerReps 25 = 10
fewerReps 10 = 8
fewerReps 8 = 5
fewerReps 5 = 3
fewerReps x = x `div` 2

{- View -}

view' :: State -> AppView Window Event
view' s = bin Window
  [ #title := "Corps Strength"
  , on #deleteEvent (const (True, Quit))
  , #widthRequest := 500
  , #heightRequest := 250
  ] appState where appState = case s of
                     GearPage -> viewGearPage
                     WorkoutPage sets -> viewWorkoutPage sets

viewGearPage :: Widget Event
viewGearPage = container Box [ #orientation := OrientationVertical, #halign := AlignCenter, #valign := AlignCenter ]
  [ widget Button [ #label := "DumbBell", on #clicked ( ChoseGear .  Just $ Dumbbell ) ]
  , widget Button [ #label := "KettleBell", on #clicked ( ChoseGear . Just $ Kettlebell ) ]
  , widget Button [ #label := "No Gear", on #clicked ( ChoseGear Nothing )]
  ]

viewWorkoutPage :: [ ExerciseSet ] -> Widget Event
viewWorkoutPage ( x:xs ) = container Box [ #orientation := OrientationVertical, #valign := AlignCenter ]
  [ widget Label [ #label := ( Text.pack . name . exercise $ x )
                 , classes [ "title" ]]
  , widget Label [ #label := ( Text.pack . show . reps $ x )
                 , classes [ "exercise_reps" ]]
  , container Box [ #orientation := OrientationHorizontal
                  , #halign := AlignCenter
                  ]
    [ widget Button
      [ #label := "Too many"
      , on #clicked ( SetWorkout $ x { reps = fewerReps . reps $ x } : xs )
      ]
    , widget Button
      [ #label := "Did it"
      , on #clicked ( SetWorkout xs )
      ]
    ]
  ]
viewWorkoutPage [] = container Box [ #orientation := OrientationVertical, #halign := AlignCenter, #valign := AlignCenter ]
  [ widget Label [ #label := "Finished.  Good Job.", classes [ "title" ]]
  , widget Button [ #label := "Quit", on #clicked Quit ]
  ]

{- Styles -}

styles :: ByteString
styles = mconcat
    [ ".title { font-size: large; margin: 5pt 10pt; }"
    , ".exercise_reps { font-size: xx-large; font-family: monospace; margin: 10pt 5pt 15pt 5pt; }"
    , "button { font-size: large; margin: 2pt 8pt; }"
    ]

{- Main -}
main :: IO ()
main = do

    void $ Gtk.init Nothing

    screen <- maybe (fail "No screen?!") return =<< Gdk.screenGetDefault
    p <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData p styles

    Gtk.styleContextAddProviderForScreen
        screen
        p
        (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

    void . async $ do
      void $ runLoop app
      Gtk.mainQuit
    Gtk.main
    where
        app = App
          { view = view'
          , update = update'
          , inputs = [ ]
          , initialState = initialstate'
          }
