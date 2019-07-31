module Db where

import qualified Crypto.Random                 as Random

import qualified Data.Aeson                    as Aeson
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Base64.URL    as Base64URL
-- import qualified Data.Either                   as Either
import qualified Data.Maybe                    as Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as TE

import qualified Database.Redis                as Redis


-- TODO use Monad fail for error handling

newtype Resources a = Resources { resources :: a }

generateId :: IO T.Text
generateId =
    let len = 32
    in  do
            bytes <- Random.getRandomBytes len :: IO B.ByteString
            return $ TE.decodeUtf8 $ B.take len $ Base64URL.encode bytes

toResources :: Aeson.FromJSON a => Either Redis.Reply [B.ByteString] -> [a]
toResources (Right xs) = Maybe.mapMaybe Aeson.decodeStrict xs
toResources _          = []

toResource
    :: Aeson.FromJSON a => Either Redis.Reply (Maybe B.ByteString) -> Maybe a
toResource (Right (Just x)) = Aeson.decodeStrict x
toResource _                = Nothing

toJson :: Aeson.ToJSON a => a -> B.ByteString
toJson = BL.toStrict . Aeson.encode

set :: Aeson.ToJSON a => Redis.Connection -> B.ByteString -> T.Text -> a -> IO (Maybe a)
set conn storeName key value = do
    response <- Redis.runRedis conn $ Redis.hset storeName (TE.encodeUtf8 key) $ toJson value
    return $ foo response where
        foo (Left _) = Nothing
        foo (Right False) = Nothing
        foo (Right True) = Just value

create
    :: Aeson.ToJSON b
    => T.Text
    -> (a -> T.Text -> b)
    -> Redis.Connection
    -> a
    -> IO (Maybe b)
create storeName toStorableValue conn newValue = do
    newId <- generateId
    set conn (TE.encodeUtf8 storeName) newId $ toStorableValue newValue newId

readAll :: Aeson.FromJSON a => T.Text -> Redis.Connection -> IO [a]
readAll storeName conn =
    Redis.runRedis conn $ toResources <$> Redis.hvals (TE.encodeUtf8 storeName)

readOne
    :: Aeson.FromJSON a => T.Text -> Redis.Connection -> T.Text -> IO (Maybe a)
readOne storeName conn key =
    Redis.runRedis conn $ toResource <$> Redis.hget storeName' key'
  where
    key'       = TE.encodeUtf8 key
    storeName' = TE.encodeUtf8 storeName
