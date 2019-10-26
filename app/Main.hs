{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- base
import Control.Exception.Base (catch, Exception, IOException)
import GHC.Generics (Generic)
import System.Exit (exitFailure)

-- dhall
import qualified Dhall as Dhall
import Dhall (inputFile)

-- optparse-applicative
import Options.Applicative

-- servant
import Servant

-- text
import qualified Data.Text as T
import Data.Text (Text)

-- warp
import Network.Wai.Handler.Warp (run)

-- Internal imports
import Button.Api

data CmdlineOptions = CmdlineOptions
  { verbose    :: Bool
  , quiet      :: Bool
  , port       :: Int
  , configFile :: FilePath
  } deriving (Show)

data ProgOptions = ProgOptions
  { verbosity  :: Dhall.Natural -- 0 is quiet, 1 is normal, 2 is verbose
  , serverPort :: Dhall.Natural -- default 8080
  } deriving (Generic, Eq, Show)

serverOptions :: Parser CmdlineOptions
serverOptions = CmdlineOptions
            <$> switch
                ( long "verbose"
               <> short 'v'
               <> help "get more output" )
            <*> switch
                ( long "quiet"
               <> short 'q'
               <> help "quiet the output" )
            <*> option auto
                ( long "port"
               <> short 'p'
               <> showDefault
               <> value 8080
               <> help "port for API"
               <> metavar "PORT" )
            <*> option auto
                ( long "config"
               <> short 'c'
               <> showDefault
               <> value "config"
               <> help "config file to use"
               <> metavar "FILE"
                )

dhProgOptT :: Dhall.Type ProgOptions
dhProgOptT = Dhall.record 
  ( ProgOptions <$> Dhall.field "verbosity" Dhall.natural
                <*> Dhall.field "port" Dhall.natural
  )

dhallConfig :: CmdlineOptions -> IO ProgOptions
dhallConfig opts = Dhall.inputFile dhProgOptT (configFile opts)

validateConfig :: CmdlineOptions -> IO (Maybe ProgOptions)
validateConfig opts = if (verbose opts && quiet opts)
    then invalidOptions
      else mkValidOpts
  where
    invalidMsg = "Verbose and Quiet are both set, this is invalid!\nPick one, -q,--quiet,-v,--verbose"
    invalidOptions = putStrLn invalidMsg >> return Nothing
    verbInt = if quiet opts then 0 else (if verbose opts then 2 else 1)
    mkValidOpts :: IO (Maybe ProgOptions)
    mkValidOpts = do
      dhconfig <- catch (dhallConfig opts)
                        (\(e :: IOError) -> return $ ProgOptions verbInt (fromIntegral $ port opts))
      if port opts /= 8080 then return (Just (dhconfig { serverPort = fromIntegral $ port opts })) else return $ Just dhconfig
     
serverButtonApi :: Server ButtonApi
serverButtonApi = undefined

apiProxy :: Proxy ButtonApi
apiProxy = Proxy

buttonApp :: Application
buttonApp = serve apiProxy serverButtonApi

runButtonApiApp :: ProgOptions -> IO ()
runButtonApiApp opts = run (fromIntegral $ serverPort opts) buttonApp
        
main :: IO ()
main = do
    opts <- validateConfig =<< execParser cmdOpts
    case opts of
      Nothing -> putStrLn "Try again." >> exitFailure
      Just o -> runButtonApiApp o
  where
    cmdOpts = info (serverOptions <**> helper)
            ( fullDesc
           <> progDesc "Publish a Button on the internet!"
           <> header "button - broadcast a message when pressed" )

