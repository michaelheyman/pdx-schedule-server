module Server where

import           Api
import           Query
import           Servant
import           Types

server :: Server ClassesAPI
server = return classList
-- [Class]

-- serverx :: Server ClassesAPI
-- serverx = do res <- findClassList
--              cls <- evaluate $ map toClass res
--              pure cls

server2 :: [Class] -> Server ClassesAPI
server2 = return

serverNonDb :: Server ClassOfferingAPI
serverNonDb = return classOfferingList

appNonDb :: Application
appNonDb = serve classOfferingAPI serverNonDb

app :: Application
app = serve classesAPI server

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
