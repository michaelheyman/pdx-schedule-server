module Server where

import           Api
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.Class (lift)
import           Query
import           Servant
import           Types

serveAPI :: Server API
serveAPI = return classList
      :<|> return pageHTML
      :<|> serveDirectoryWebApp "static-files"

server :: Server API
server = liftIO getClasses
      :<|> return pageHTML
      :<|> serveDirectoryWebApp "../dist/resources"

server2 :: [Class] -> Server ClassesAPI
server2 = return

serverNonDb :: Server ClassOfferingAPI
serverNonDb = return classOfferingList

appNonDb :: Application
appNonDb = serve classOfferingAPI serverNonDb

server7 :: Server StaticAPI
server7 = serveDirectoryWebApp "static-files"

server8 :: Server MyApi
server8 = serveDirectoryWebApp "/var/www"

app3 :: Application
app3 = serve staticAPI server7

app4 :: Application
app4 = serve myapi server8

app5 :: Application
app5 = serve testapi server100

-- app6 :: Application
-- app6 = serve rootapi server123

--server200 :: Server ClassesAPI
--server200 = return _

appAPI :: Application
appAPI = serve api serveAPI
