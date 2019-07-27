{-# LANGUAGE OverloadedStrings     #-}

import           Data.ByteString
import           Database.Redis
import qualified Network.Wai.Handler.Warp      as Warp
import qualified Api


connectToRedis :: IO Connection
connectToRedis = checkedConnect defaultConnectInfo

getUsers :: Connection -> IO (Either Reply [ByteString])
getUsers conn = runRedis conn $ hvals "users"

main :: IO ()
main = do
    conn  <- connectToRedis
    users <- getUsers conn
    print users
