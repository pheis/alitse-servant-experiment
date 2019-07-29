{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Accounts where

import Prelude hiding (id)

import           Control.Monad.IO.Class

import qualified Data.Aeson                    as Aeson
import qualified Data.Proxy                    as Proxy
import qualified Data.Text                     as T

import qualified Database.Redis                as Redis

import           GHC.Generics                   ( Generic )

import           Servant ( Application, Handler, Server, serve)
import           Servant.API
import qualified Lib


api :: Proxy.Proxy Api
api = Proxy.Proxy

type Api = "accounts" :> ReqBody '[JSON] NewAccount :> Post '[JSON] Account
    :<|>"accounts" :> Get '[JSON] Accounts
server :: Redis.Connection -> Server Api
server conn = postAccount conn :<|> getAccounts conn
--
app :: Redis.Connection -> Application
app conn = serve api $ server conn


accountStoreName :: T.Text
accountStoreName = "Accounts"

readAccount :: Redis.Connection -> T.Text -> IO (Maybe Account)
readAccount = Lib.readOne accountStoreName

readAccounts :: Redis.Connection -> IO [Account]
readAccounts = Lib.readAll accountStoreName

createAccount :: Redis.Connection -> NewAccount -> IO Account
createAccount = Lib.create accountStoreName toStorable

postAccount :: Redis.Connection -> NewAccount -> Handler Account
postAccount conn newAccount = do
    storedAccount <- liftIO (createAccount conn newAccount)
    return storedAccount

getAccounts :: Redis.Connection -> Handler Accounts
getAccounts conn = do
    accounts <- liftIO (readAccounts conn)
    return $ Accounts accounts

getAccount :: Redis.Connection -> T.Text -> Handler (Maybe Account)
getAccount conn accountId = do
    account <- liftIO (readAccount conn accountId)
    return account


-- DATATYPES
--
data Role =  Mentee | Mentor | Admin deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Role
instance Aeson.ToJSON Role

data Account = Account
    { id :: T.Text
    , login_name :: T.Text
    , role  :: Role
    , email :: T.Text
    , phone :: T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Account
instance Aeson.ToJSON Account

newtype Accounts = Accounts {
    resources :: [Account]
} deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Accounts
instance Aeson.ToJSON Accounts


data NewAccount = NewAccount
    { login_name :: T.Text
    , role  :: Role
    , email :: Maybe T.Text
    , phone :: Maybe T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON NewAccount
instance Aeson.ToJSON NewAccount

toStorable :: NewAccount -> T.Text -> Account
toStorable NewAccount { login_name, role, email, phone } newId = Account {
    id = newId,
    login_name = login_name,
    role = Mentee,
    phone = "asdf",
    email = "lol"
}
