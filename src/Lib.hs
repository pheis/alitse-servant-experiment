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

fromJsons :: Aeson.FromJSON a => Either Redis.Reply [B.ByteString] -> [a]
fromJsons (Right xs) = Maybe.mapMaybe Aeson.decodeStrict xs
fromJsons _                   = []

toJson :: Aeson.ToJSON a => a -> B.ByteString
toJson = BL.toStrict . Aeson.encode


hset :: Aeson.ToJSON a => Redis.Connection -> B.ByteString -> T.Text -> a -> IO a
hset conn storeName key value = do
    _ <- Redis.runRedis conn $ Redis.hset storeName (TE.encodeUtf8 key) $ toJson value
    return value

create :: Aeson.ToJSON b => B.ByteString -> (a -> T.Text -> b) -> Redis.Connection -> a -> IO b
create storeName toStorableValue conn newValue = do
    newId <- generateId
    hset conn storeName newId $ toStorableValue newValue newId

readAll :: Aeson.FromJSON a => B.ByteString -> Redis.Connection -> IO [a]
readAll storeName conn = Redis.runRedis conn $ fromJsons <$> Redis.hvals storeName

-- readOne :: Aeson.FromJSON a => B.ByteString -> Redis.Connection -> IO [a]
-- readOne storeName conn = Redis.runRedis conn $ fromJsons <$> Redis.hget storeName
