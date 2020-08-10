module App.Types
  ( User (..),
    Login (..),
    RootPage (..),
  )
where

import Data.Aeson
import Lucid as L
import Lucid.Base as L
import Protolude
import Servant.API
import Servant.HTML.Lucid

data User = User
  { userId :: Int,
    userEmail :: Text
  }
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

data Login = Login
  { username :: Text,
    password :: Text
  }
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

newtype RootPage = RootPage (Html ()) deriving (Generic)

instance ToHtml RootPage where
  toHtmlRaw = L.toHtml

  toHtml (RootPage x) = relaxHtmlT x

instance MimeUnrender HTML RootPage where
  mimeUnrender _ _ = panic "should not be unrendered by a client."
