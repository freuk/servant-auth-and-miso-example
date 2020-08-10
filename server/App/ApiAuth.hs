module App.ApiAuth
  ( AuthAPI,
    Protected,
  )
where

import App.Api
import App.Types
import Servant.API
import Servant.Auth

type Protected =
  Auth '[Cookie] User
    :> Unprotected

type AuthAPI = Protected :<|> Public
