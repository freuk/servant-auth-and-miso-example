module App.ApiAuth
  ( AuthAPI,
    Protected,
  )
where

import App.Types
import Servant.API
import Servant.Auth
import Servant.Auth.Server

type Protected =
  Auth '[Cookie] User
    :> Unprotected

type AuthAPI = Protected :<|> Public
