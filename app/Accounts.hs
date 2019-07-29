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



-- app :: Redis.Connection -> Application
-- app conn = serve api $ server conn

-- DATATYPES

data Role =  Mentee | Mentor | Admin deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON Role
instance Aeson.ToJSON Role

data StorableAccount = StorableAccount
    { id :: T.Text
    , login_name :: T.Text
    , role  :: Role
    , email :: T.Text
    , phone :: T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON StorableAccount
instance Aeson.ToJSON StorableAccount

data NewAccount = NewAccount
    { login_name :: T.Text
    , role  :: Role
    , email :: Maybe T.Text
    , phone :: Maybe T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON NewAccount
instance Aeson.ToJSON NewAccount

toStorable :: NewAccount -> T.Text -> StorableAccount
toStorable NewAccount { login_name, role, email, phone } newId = StorableAccount {
    id = newId,
    login_name = login_name,
    role = Mentee,
    phone = "asdf",
    email = "lol"
}

accountStoreName :: T.Text
accountStoreName = "Accounts"

readAccount :: Redis.Connection -> T.Text -> IO (Maybe StorableAccount)
readAccount = Lib.readOne accountStoreName
readAccounts = Lib.readAll accountStoreName
createAccount = Lib.create accountStoreName toStorable

postAccount :: Redis.Connection -> NewAccount -> Handler StorableAccount
postAccount conn newAccount = do
    storedAccount <- liftIO (createAccount conn newAccount)
    return storedAccount

getAccounts :: Redis.Connection -> Handler [StorableAccount]
getAccounts conn = do
    accounts <- liftIO (readAccounts conn)
    return accounts

getAccount :: Redis.Connection -> T.Text -> Handler (Maybe StorableAccount)
getAccount conn accountId = do
    account <- liftIO (readAccount conn accountId)
    return account

---
-- API DEFINITION
--
--      :<|> "accounts" :> Capture "id" :> Get '[JSON] [StorableAccount]

-- server :: Redis.Connection -> Server Api
-- server conn = liftIO (createAccount conn)

api :: Proxy.Proxy Api
api = Proxy.Proxy

type Api = "accounts" :> ReqBody '[JSON] NewAccount :> Post '[JSON] StorableAccount
    :<|>"accounts" :> Get '[JSON] [StorableAccount]
server :: Redis.Connection -> Server Api
server conn = (postAccount conn) :<|> (getAccounts conn)
--
app :: Redis.Connection -> Application
app conn = serve api $ server conn
