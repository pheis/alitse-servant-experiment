{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Accounts where

import Prelude hiding (id)

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as B
import qualified Data.Proxy                    as Proxy
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

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

type Api = "accounts" :> ReqBody '[JSON] NewAccount :> Post '[JSON] StorableAccount
    :<|>"accounts" :> Get '[JSON] [StorableAccount]



accountStoreName :: B.ByteString
accountStoreName = "Accounts"

postAccount :: Redis.Connection -> NewAccount -> Handler StorableAccount
postAccount conn newAccount = do
    storedAccount <- liftIO (Lib.create accountStoreName (toStorable) conn newAccount)
    return storedAccount

getAccounts :: Redis.Connection -> Handler [StorableAccount]
getAccounts conn = do
    accounts <- liftIO (Lib.readAll accountStoreName conn)
    return accounts

---
-- API DEFINITION
--
--      :<|> "accounts" :> Capture "id" :> Get '[JSON] [StorableAccount]

-- server :: Redis.Connection -> Server Api
-- server conn = liftIO (createAccount conn)

api :: Proxy.Proxy Api
api = Proxy.Proxy
--
--
server :: Redis.Connection -> Server Api
server conn = (postAccount conn) :<|> (getAccounts conn)
--
app :: Redis.Connection -> Application
app conn = serve api $ server conn
