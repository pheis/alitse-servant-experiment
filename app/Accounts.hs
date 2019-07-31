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

import           Servant
import           Servant.API

import qualified Db
import qualified Identity


api :: Proxy.Proxy Api
api = Proxy.Proxy

type Api = "accounts" :> ReqBody '[JSON] NewAccountWithPassword :> Post '[JSON] Account
    :<|> "accounts" :> Get '[JSON] Accounts
    :<|> "accounts" :> Capture "id" T.Text :> Get '[JSON] Account
server :: Redis.Connection -> Server Api
server conn = postAccount conn :<|> getAccounts conn :<|> getAccount conn
--
app :: Redis.Connection -> Application
app conn = serve api $ server conn


-- Handlers
--
postAccount :: Redis.Connection -> NewAccountWithPassword -> Handler Account
postAccount conn newAccount = do
    maybeStoredAccount <- liftIO (createAccount conn newAccount)
    case maybeStoredAccount of
        Just account -> return account
        Nothing -> throwError $ err500 { errBody = "We broken, dunno why" }

getAccounts :: Redis.Connection -> Handler Accounts
getAccounts conn = do
    accounts <- liftIO (readAccounts conn)
    return $ Accounts accounts

getAccount :: Redis.Connection -> T.Text -> Handler Account
getAccount conn accountId = do
    maybeAccount <- liftIO (readAccount conn accountId)
    case maybeAccount of
        Just account -> return account
        Nothing -> throwError $ err404 { errBody = "(╯°□°）╯︵ ┻━┻)." }


-- DB
--
storeName :: T.Text
storeName = "Accounts"

readAccount :: Redis.Connection -> T.Text -> IO (Maybe Account)
readAccount = Db.readOne storeName

readAccounts :: Redis.Connection -> IO [Account]
readAccounts = Db.readAll storeName

createAccount :: Redis.Connection -> NewAccountWithPassword -> IO (Maybe Account)
createAccount = Db.create storeName toStorable

--
-- DATATYPES
--
data Account = Account
    { id :: T.Text
    , login_name :: T.Text
    , role  :: Identity.Role
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
    , role  :: Identity.Role
    , email :: Maybe T.Text
    , phone :: Maybe T.Text
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON NewAccount
instance Aeson.ToJSON NewAccount

data NewAccountWithPassword = NewAccountWithPassword
    { password :: T.Text
    , account :: NewAccount
    } deriving (Eq, Show, Read, Generic)
instance Aeson.FromJSON NewAccountWithPassword
instance Aeson.ToJSON NewAccountWithPassword


-- Data conversions
--
_account :: NewAccountWithPassword -> NewAccount
_account NewAccountWithPassword { account} = account

newAccountToStorable :: NewAccount -> T.Text -> Account
newAccountToStorable NewAccount { login_name, role, email, phone } newId = Account {
    id = newId,
    login_name = login_name,
    role = Identity.Mentee,
    phone = "asdf",
    email = "lol"
}

toStorable account newId = newAccountToStorable (_account account) newId
