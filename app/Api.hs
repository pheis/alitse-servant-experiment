{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Api where

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

helloApi :: Proxy.Proxy HelloAPI
helloApi = Proxy.Proxy

users :: [User]
users = [User "123-1234" "Janteri" Mentor, User "1234-1324" "Janteri" Mentee]


userStoreName :: BS.ByteString
userStoreName = "users"

server :: Server UsersAPI
server = return users

app :: Application
app = serve helloApi server

connectToRedis :: IO Redis.Connection
connectToRedis = Redis.checkedConnect Redis.defaultConnectInfo

-- bsToUser :: Aeson.FromJSON a => BS.ByteString -> Maybe a
-- bsToUser bs = Aeson.decodeStrict bs
--
-- foofuu :: (Monad m, Aeson.FromJSON a, Functor f) =>
--             m (f BS.ByteString) -> m (f (Maybe a))
-- -- foofuu vals = fmap Aeson.decodeStrict <$> vals
--
getUsers2 :: Redis.Redis (Either Redis.Reply [Maybe User])
getUsers2 =
    fmap (fmap (fmap (fmap Aeson.decodeStrict))) Redis.hvals userStoreName

-- toUsers :: Either Redis.Reply [BS.ByteString] -> [Maybe User]
-- toUsers (Right bsList) = fmap Aeson.decodeStrict bsList
-- toUsers _ = []
--
toUsers :: Either Redis.Reply [BS.ByteString] -> [User]
toUsers (Right bsList) = Maybe.mapMaybe Aeson.decodeStrict bsList
toUsers _              = []

getUsers3 :: Redis.Redis [User]
getUsers3 = toUsers <$> Redis.hvals userStoreName

getUsers :: Redis.Connection -> IO (Either Redis.Reply [BS.ByteString])
getUsers conn = Redis.runRedis conn $ Redis.hvals userStoreName



-- foofuu :: IO (Either Redis.Reply [BS.ByteString]) -> IO (Either Redis.Reply [Char])


-- userToText :: User -> T.Text
maikkeli :: User
maikkeli = User "asdf" "Maikkeli" Mentor

userToBs :: User -> BS.ByteString
userToBs user = BL.toStrict $ Aeson.encode user

setUser :: Redis.Connection -> User -> IO (Either Redis.Reply Bool)
setUser conn user =
    Redis.runRedis conn $ Redis.hset userStoreName "asdf" $ userToBs user


setMaikkeli :: Redis.Connection -> IO (Either Redis.Reply Bool)
setMaikkeli conn = setUser conn maikkeli
