import qualified Network.Wai.Handler.Warp      as Warp
import qualified Api
import qualified Accounts




main :: IO ()
main = do
    conn  <- Api.connectToRedis
    _ <- Api.setMaikkeli conn
    users <- Api.readUsers conn
    print users
    Warp.run 8081 $ Accounts.app conn

-- main :: IO ()
-- main = do
--     bytes <- Api.generateAlitseId
--     print bytes
