module Main (main) where

import           Api
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant                     (serve)
import           Server

main :: IO ()
main = do
  putStrLn ("Running server: " ++ "http://localhost:" ++ show port
           ++ "\nRunning API: " ++ "\thttp://localhost:" ++ show port ++ "/classes")
  run port (simpleCors app)
 where
  app  = serve api server
  port = 8080
