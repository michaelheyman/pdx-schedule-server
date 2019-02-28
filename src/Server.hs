module Server where

import           Api
-- import           Model
import           Control.Exception.Base
import           Network.Wai.Handler.Warp (run)
import           Query
import           Servant                  (Application, Server, serve)

-- server2 :: Server allAPI
-- server2 = return allAPI

server :: Server ClassesAPI
server = return classList

server2 :: [Class] -> Server ClassesAPI
server2 c = return c

serverNonDb :: Server ClassOfferingAPI
serverNonDb = return classOfferingList

appNonDb :: Application
appNonDb = serve classOfferingAPI serverNonDb

app :: Application
app = serve classesAPI server

main :: IO ()
main = do
  putStrLn "Running server on http://localhost:8081/classes"
  res <- findClassInstances
  cls <- evaluate $ map toClass res
  run 8081 (app cls)
 where
  app :: [Class] -> Application
  app cls = serve classesAPI (server2 cls)

