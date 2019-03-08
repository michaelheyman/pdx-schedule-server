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
  res <- findClassList
  cls <- evaluate $ map toClass res
  run port (simpleCors $ app cls)
 where
  app cls = serve classesAPI (server2 cls)
  port = 8080

testmain :: IO ()
testmain = do
  putStrLn ("Running server on http://localhost:" ++ show port ++ "/")
  run port (simpleCors app5)
 where
   port = 8080
