{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api where

import           Control.Monad.IO.Class
import           Control.Applicative            ( (<$>) )
import qualified Data.Maybe                    as Maybe
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BL
import qualified Database.Redis                as Redis
import qualified Data.Aeson                    as Aeson
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
instance Aeson.FromJSON Role
instance Aeson.ToJSON Role

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
instance Aeson.FromJSON User
instance Aeson.ToJSON User

-- type UserAPI = "users" :> Capture "id" T.Text :> Get '[JSON] [User]
type UsersAPI = "users" :> Get '[JSON] [User]

type HelloAPI = UsersAPI

api :: Proxy.Proxy HelloAPI
api = Proxy.Proxy

users :: [User]
users = [User "123-1234" "Janteri" Mentor, User "1234-1324" "Janteri" Mentee]


userStoreName :: BS.ByteString
userStoreName = "users"

server :: Redis.Connection -> Server UsersAPI
server conn = liftIO (readUsers conn)

app :: Redis.Connection -> Application
app conn = serve api $ server conn

connectToRedis :: IO Redis.Connection
connectToRedis = Redis.checkedConnect Redis.defaultConnectInfo

toUsers :: Either Redis.Reply [BS.ByteString] -> [User]
toUsers (Right byteStrings) = Maybe.mapMaybe Aeson.decodeStrict byteStrings
toUsers _                   = []

readUsers :: Redis.Connection -> IO [User]
readUsers conn = Redis.runRedis conn $ toUsers <$> Redis.hvals userStoreName

maikkeli :: User
maikkeli = User "asdf" "Maikkeli" Mentor

userToBs :: User -> BS.ByteString
userToBs user = BL.toStrict $ Aeson.encode user

setUser :: Redis.Connection -> User -> IO (Either Redis.Reply Bool)
setUser conn user =
    Redis.runRedis conn $ Redis.hset userStoreName "asdf" $ userToBs user


setMaikkeli :: Redis.Connection -> IO (Either Redis.Reply Bool)
setMaikkeli conn = setUser conn maikkeli
