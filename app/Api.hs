{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
module Api where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Proxy as Proxy
import qualified Data.Text                  as T
import qualified GHC.Generics as Generics
import Servant.API ( Capture, Get, PlainText, (:>), (:<|>), JSON)

data User = User
    { name :: T.Text
    , age  :: Int
    } deriving (Eq, Show, Read, Generics.Generic)
instance FromJSON User
instance ToJSON User

type HelloAPI  = Get '[PlainText] T.Text
            :<|> "user" :> Capture "name" T.Text :> Capture "age" Int :> Get '[JSON] User

helloApi :: Proxy.Proxy HelloAPI
helloApi = Proxy.Proxy
