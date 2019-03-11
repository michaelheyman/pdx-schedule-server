module Main (main) where

import           Api
import           Control.Exception.Base      (evaluate)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Query
import           Servant                     (serve)
import           Server
import           Types

main :: IO ()
main = do
  putStrLn ("Running server on http://localhost:" ++ show port ++ "/classes")
  -- res <- findClassList
  -- cls <- evaluate $ map toClass res
  -- cls <- getClasses
  -- run port (simpleCors $ app cls)
  run port (simpleCors app)
 where
  -- app cls = serve classesAPI (server2 cls)
  -- app cls = serve api serveAPI
  app  = serve classesAPI serverP
  port = 8080

testmain :: IO ()
testmain = do
  putStrLn ("Running server on http://localhost:" ++ show port ++ "/")
  run port (simpleCors appAPI)
 where
  port = 8080
