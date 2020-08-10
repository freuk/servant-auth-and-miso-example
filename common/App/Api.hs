{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Api
  ( Unprotected,
    Public,
  )
where

import App.Types
import Protolude
import Servant.API
import Servant.Docs hiding (API)
import Servant.HTML.Lucid
import Web.Cookie

type Unprotected =
  ( "die" :> "roll" :> Get '[JSON] Int
      :<|> "loggedin" :> Get '[JSON] User
  )

type Public =
  Get '[HTML] RootPage
    :<|> "login"
    :> ReqBody '[JSON] Login
    :> PostNoContent
         '[JSON]
         ( Headers
             '[ Header "Set-Cookie" SetCookie,
                Header "Set-Cookie" SetCookie
              ]
             NoContent
         )
      :<|> "static"
    :> Raw

instance ToSample SetCookie where
  toSamples _ =
    singleSample $
      defaultSetCookie
        { setCookieName = "cookieName",
          setCookieValue = "cookieValue"
        }

instance ToSample Int where
  toSamples _ =
    singleSample 1

instance ToSample Login where
  toSamples _ =
    singleSample $ Login {username = "foo", password = "bar"}

instance ToSample User where
  toSamples _ =
    singleSample $
      User
        { userId = 1,
          userEmail = "foo@bar"
        }

instance ToSample RootPage where
  toSamples _ =
    singleSample $ RootPage "<html></html>"
