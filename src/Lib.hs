module Lib where

import qualified Crypto.Random as Random

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Base64.URL as Base64URL
import qualified Data.Either                   as Either
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import qualified Database.Redis                as Redis

generateId :: IO T.Text
generateId = let len = 32 in do
    bytes <- Random.getRandomBytes len :: IO B.ByteString
    return $ TE.decodeUtf8 $ B.take len $ Base64URL.encode bytes

listFromRedisReply :: Aeson.FromJSON a => Either Redis.Reply [B.ByteString] -> [a]
listFromRedisReply (Right xs) = Maybe.mapMaybe Aeson.decodeStrict xs
listFromRedisReply _          = error "something went wrong"

fromRedisReply :: Aeson.FromJSON a => Either Redis.Reply (Maybe B.ByteString) -> Maybe a
fromRedisReply (Right (Just x)) = Aeson.decodeStrict x
fromRedisReply _              = error "something went wrong"

-- fromJson x = fmap (fmap Aeson.decodeStrict) x


toJson :: Aeson.ToJSON a => a -> B.ByteString
toJson = BL.toStrict . Aeson.encode


hset :: Aeson.ToJSON a => Redis.Connection -> B.ByteString -> T.Text -> a -> IO a
hset conn storeName key value = do
    _ <- Redis.runRedis conn $ Redis.hset storeName (TE.encodeUtf8 key) $ toJson value
    return value

create :: Aeson.ToJSON b => T.Text -> (a -> T.Text -> b) -> Redis.Connection -> a -> IO b
create storeName toStorableValue conn newValue = do
    newId <- generateId
    hset conn (TE.encodeUtf8 storeName) newId $ toStorableValue newValue newId

readAll :: Aeson.FromJSON a => T.Text -> Redis.Connection -> IO [a]
readAll storeName conn = Redis.runRedis conn $ listFromRedisReply <$> Redis.hvals (TE.encodeUtf8 storeName)

readOne :: Aeson.FromJSON a => T.Text -> Redis.Connection -> T.Text -> IO (Maybe a)
readOne storeNameText conn keyText = Redis.runRedis conn $ fromRedisReply <$> Redis.hget storeName key
    where key = TE.encodeUtf8 keyText
          storeName = TE.encodeUtf8 storeNameText
