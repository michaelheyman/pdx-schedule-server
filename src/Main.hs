module Main (main) where

import           Api
import           Control.Exception.Base      (evaluate)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Query
import           Servant                     (serve)
import           Server

main :: IO ()
main = do
  putStrLn ("Running server on http://localhost:" ++ show port ++ "/classes")
  run port (simpleCors app)
 where
  app  = serve api server
  port = 8080

testmain :: IO ()
testmain = do
  putStrLn ("Running server on http://localhost:" ++ show port ++ "/")
  run port (simpleCors appAPI)
 where
  port = 8080
