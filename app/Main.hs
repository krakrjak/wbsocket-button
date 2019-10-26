module Main where

-- base
import Data.Default

-- Internal imports
import Button.Api

data CmdlineOptions = CmdlineOptions
  { verbose    :: Bool
  , quiet      :: Bool
  , configFile :: Maybe FilePath
  }

instance Default CmdlineOptions where
  def = CmdlineOptions False False Nothing

main :: IO ()
main = someFunc
