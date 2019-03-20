{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Api
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text, empty)
import           Globals                (bootstrapUrl, fontAwesomeUrl)
import           Lucid
import           Query
import           Servant

server :: Server API
server = liftIO getClasses
    :<|> return pageHtml
    :<|> serveDirectoryWebApp "dist/resources"

pageHtml :: Html ()
pageHtml = do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    meta_ [ name_    "viewport"
          , content_ "width=device-width, initial-scale=1, shrink-to-fit=no"
          ]
    script_ [ src_ "static/main.js" ] empty
    link_ [ rel_ "stylesheet"
          , href_ bootstrapUrl ]
    link_ [ rel_ "stylesheet"
          , href_ fontAwesomeUrl ]
  body_ $ do
    div_ [ id_ "elm"] ""
    script_ [] ("var app = Elm.Main.init({                 \n\
                 \    node: document.getElementById('elm') \n\
                 \});" :: Text)
