{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import qualified Data.Proxy                    as Proxy
import qualified Data.Text                     as T
import           GHC.Generics                   ( Generic )
import           Servant                        ( Application
                                                , Server
                                                , serve
                                                )
import           Servant.API                    ( Capture
                                                , Get
                                                , (:>)
                                                , (:<|>)
                                                , JSON
                                                )


data Role = Mentee | Mentor | Admin deriving (Eq, Show, Read, Generic)
instance FromJSON Role
instance ToJSON Role

data Account = Account
    { login_name :: T.Text
    , role  :: Role
    , email :: Maybe T.Text
    , phone :: Maybe T.Text
    } deriving (Eq, Show, Read, Generic)

data User = User
    { account_id :: T.Text
    , display_name :: T.Text
    , role :: Role
    } deriving (Eq, Show, Read, Generic)
instance FromJSON User
instance ToJSON User

-- type UserAPI = "users" :> Capture "id" T.Text :> Get '[JSON] [User]
type UsersAPI = "users" :> Get '[JSON] [User]

type HelloAPI = UsersAPI

helloApi :: Proxy.Proxy HelloAPI
helloApi = Proxy.Proxy

users :: [User]
users = [User "123-1234" "Janteri" Mentor, User "1234-1324" "Janteri" Mentor]



server :: Server UsersAPI
server = return users

app :: Application
app = serve helloApi server
