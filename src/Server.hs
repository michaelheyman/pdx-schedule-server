module Server where

import           Api
-- import           Model
import           Network.Wai.Handler.Warp (run)
import           Query
import           Servant                  (Application, Server, serve)

-- server2 :: Server allAPI
-- server2 = return allAPI

-- serverNonDb :: Server ClassOfferingAPI
-- serverNonDb = return classoffering2

-- serverNonDb :: Server ClassOfferingAPI
-- serverNonDb = return classoffering2

--app :: Application
--app = serve classOfferingAPI server

--app2 :: Application
--app2 = serve classOfferingAPI server

main :: IO ()
main = do
  putStrLn "Running server on http://localhost:8081/classes"
  -- run 8081 app
