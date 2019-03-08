{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api where

import           Control.Monad.IO.Class
import           Data.ByteString.Lazy   as BS
import           Data.Text              (Text)
import           Database.Schema
import           Lucid
import           Lucid.Base             (makeAttribute)
import           Lucid.Servant
import           Query
import           Servant
import           Servant.HTML.Lucid
import           Types

type ClassOfferingAPI = "classes" :> Get '[JSON] [ClassOffering]
type ClassesAPI = "classes" :> Get '[JSON] [Class]

classOfferingAPI :: Proxy ClassOfferingAPI
classOfferingAPI = Proxy :: Proxy ClassOfferingAPI

classesAPI :: Proxy ClassesAPI
classesAPI = Proxy :: Proxy ClassesAPI

type StaticAPI = "static" :> Raw

staticAPI :: Proxy StaticAPI
staticAPI = Proxy

type API =
    "classes" :> Get '[JSON] [Class] :<|>
    Raw

api :: Proxy API
api = Proxy

type MyApi = "static" :> Raw

myapi :: Proxy MyApi
myapi = Proxy :: Proxy MyApi

-- type TestAPI = "string" :> Get '[HTML] String
--     :<|> "nested" :> "html" :> Get '[HTML] (Html ())
type TestAPI = "string" :> Get '[HTML] String :<|>
               Get '[HTML] (Html ())

testapi :: Proxy TestAPI
testapi = Proxy

server100 :: Server TestAPI
server100 = return "example" :<|> return html
  where
    html :: Html ()
    html = do
       p_ $ b_ "bar"
       p_ $ i_ "iik"

type RootAPI = Get '[HTML] RawHtml

newtype RawHtml = RawHtml { unRaw :: BS.ByteString }

-- tell Servant how to render the newtype to html page, in this case simply unwrap it
instance MimeRender HTML RawHtml where
     mimeRender _ =  unRaw

rootapi :: Proxy RootAPI
rootapi = Proxy

server123 :: Server RootAPI
server123 = fmap RawHtml (liftIO $ BS.readFile "your/file/path.html")

pageHTML :: Html ()
pageHTML = do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    meta_ [ name_    "viewport"
          , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
          ]
    script_ [ src_ "main.js" ] ("" :: Text)
    link_ [ rel_ "stylesheet"
          , href_ "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css" ]
    link_ [ rel_ "stylesheet"
          , href_ "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css" ]
  body_ $ do
    div_ [ id_ "elm"] $ do
      p_ "Example"
      p_ "Example"
    script_ [] ("var app = Elm.Main.init({              \n\
                 \    node: document.getElementById('elm') \n\
                 \});" :: Text)
