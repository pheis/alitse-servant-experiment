import qualified Network.Wai.Handler.Warp      as Warp
import qualified Api

main :: IO ()
main = do
    conn  <- Api.connectToRedis
    _ <- Api.setMaikkeli conn
    users <- Api.readUsers conn
    print users
    Warp.run 8081 $ Api.app conn
