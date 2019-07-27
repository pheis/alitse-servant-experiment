-- import qualified Network.Wai.Handler.Warp      as Warp
import qualified Api

main :: IO ()
main = do
    conn  <- Api.connectToRedis
    _ <- Api.setMaikkeli conn
    users <- Api.getUsers conn
    print users
