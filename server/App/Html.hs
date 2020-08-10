{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Html
  ( basePage,
  )
where

import App.Types
import qualified Lucid as L
import qualified Lucid.Base as L
import Protolude

basePage :: RootPage
basePage =
  RootPage . L.html_ [L.lang_ "en"] $ do
    L.head_ $ do
      L.title_ "app"
      L.meta_ [L.charset_ "utf-8"]
      L.meta_
        [ L.name_ "viewport",
          L.content_ "width=device-width, initial-scale=1, maximum-scale=1"
        ]
    jsRefAsync "static/all.js"
  where
    jsRefAsync :: (Monad m) => Text -> L.HtmlT m ()
    jsRefAsync href =
      L.with
        (L.script_ mempty)
        [ L.makeAttribute "src" href
        ]
