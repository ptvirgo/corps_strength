module Main where

import Control.Monad
import Options.Applicative
import Data.Semigroup ((<>))

import Core
import Exercises

parseKettlebell :: Parser (Maybe Gear)
parseKettlebell = flag' (Just Kettlebell)
  ( long "kettlebell"
  <> short 'k'
  <> help "use a kettlebell"
  )

parseDumbbell :: Parser (Maybe Gear)
parseDumbbell = flag' (Just Dumbbell)
  ( long "dumbbell"
  <> short 'd'
  <> help "use a dumbbell"
  )

gearInput :: Parser (Maybe Gear)
gearInput = parseKettlebell <|> parseDumbbell <|> pure Nothing

opts :: ParserInfo (Maybe Gear)
opts = info (gearInput <**> helper)
  ( fullDesc <> progDesc "Generate a corps strength mission." )

main :: IO ()
main = do
  g <- execParser opts
  mission <-
        case g of
          Just g' -> makeGearedMission g'
          Nothing -> makeGearlessMission
  forM_ mission print
