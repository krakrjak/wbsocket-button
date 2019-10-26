{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Button.Api
    ( ButtonApi
    , someFunc
    ) where

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
  }
instance Default ButtonMetadata where
  def = ButtonMetadata 0 Nothing

-- Requests on /button
data ButtonAction
    = Push
    | Reset
    | Peek
instance Default ButtonAction where
  def = Peek

-- Respones to posting to /button
data ButtonReaction
    = Pushed
    | Cleared
    | Examined
    | None
instance Default ButtonReaction where
  def = None

type ButtonApi = "button" :> Get '[JSON] ButtonMetadata
            :<|> "button" :> ReqBody '[JSON] ButtonAction :> Post '[JSON] ButtonReaction

someFunc :: IO ()
someFunc = putStrLn "someFunc"
