module Server where

import           Api
import           Model
import           Network.Wai.Handler.Warp (run)
import           Query
import           Servant                  (Application, Server, serve)

server :: Server NewAPI
server = return classoffering2

app :: Application
app = serve classOfferingAPI server

main :: IO ()
main = do
  putStrLn "Running server on http://localhost:8081/classes"
  run 8081 app
