{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Button.Api
    ( ButtonApi
    , AliveApi
    , ButtonMetadata(..)
    , ButtonAction(..)
    , ButtonReaction(..)
    , someFunc
    ) where

-- base
import GHC.Generics (Generic)

-- aeson
import Data.Aeson as J

-- data-default
import Data.Default (Default, def)

-- time
import Data.Time.Clock (UTCTime)

-- servant
import Servant.API

-- The button has some state...
data ButtonMetadata = ButtonMetadata
  { subs :: Int
  , lastPushed :: Maybe UTCTime
  } deriving (Generic, Show)
instance Default ButtonMetadata where
  def = ButtonMetadata 0 Nothing
instance J.FromJSON ButtonMetadata
instance J.ToJSON ButtonMetadata

-- Requests on /button
data ButtonAction = Peek | Poke
  deriving (Generic, Enum, Show)
instance J.FromJSON ButtonAction
instance J.ToJSON ButtonAction

-- Respones to posting to /button
data ButtonReaction
    = Peeked | Poked
  deriving (Generic, Enum, Show)
instance J.FromJSON ButtonReaction
instance J.ToJSON ButtonReaction

type AliveApi = Get '[JSON] ()

type ButtonApi = "button" :> Get '[JSON] ButtonMetadata
            :<|> "button" :> ReqBody '[JSON] ButtonAction :> Post '[JSON] ButtonReaction

someFunc :: IO ()
someFunc = putStrLn "someFunc"
