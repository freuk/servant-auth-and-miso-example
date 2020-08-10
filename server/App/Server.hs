{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-missing-local-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Server (main) where

import App.Api
import App.ApiAuth
import App.Html
import App.Types
import Control.Monad.Trans (liftIO)
import qualified Data.Time as Time
import Network.Wai.Handler.Warp (run)
import Protolude
import Servant
import Servant.Auth.Server
import System.Directory
import System.Random

deriving instance ToJWT User

deriving instance FromJWT User

staticDir :: FilePath
staticDir = "static"

server :: CookieSettings -> JWTSettings -> Server AuthAPI
server cs jwts = protected :<|> public cs jwts

protected :: AuthResult User -> Server Unprotected
protected (Authenticated user) = dice :<|> login
  where
    dice :: Handler Int
    dice = liftIO (randomRIO (1, 6))
    login = do
      putText ("login request with data" <> show user)
      return (User 1 "test@test.com")
protected _ = throwAll err401

public :: CookieSettings -> JWTSettings -> Server Public
public cs jwts =
  pure basePage :<|> loginH :<|> staticH
  where
    loginH = checkCreds cs jwts
    staticH = serveDirectoryWebApp staticDir

checkCreds ::
  CookieSettings ->
  JWTSettings ->
  Login ->
  Handler
    ( Headers
        '[ Header "Set-Cookie" SetCookie,
           Header "Set-Cookie" SetCookie
         ]
        NoContent
    )
checkCreds
  cookieSettings
  jwtSettings
  l@(Login loginUserIdent loginUserPassword) = do
    putText ("received api call with: " <> show l)
    let mUser =
          if loginUserIdent == "test@test.com"
            && loginUserPassword == "password"
            then Just $ User 1 "test@test.com"
            else Nothing
    case mUser of
      Nothing -> throwError err401
      Just usr -> do
        mApplyCookies <-
          liftIO $
            acceptLogin cookieSettings jwtSettings usr
        case mApplyCookies of
          Nothing -> throwError err401
          Just applyCookies ->
            return $ applyCookies NoContent

main :: IO ()
main = do
  getCurrentDirectory >>= print
  myKey <- generateKey
  now <- Time.getCurrentTime
  let jwtCfg = defaultJWTSettings myKey
      cookieSettings =
        defaultCookieSettings
          { cookiePath = Just "/",
            cookieExpires =
              Just
                now
                  { Time.utctDay = Time.addDays 30 (Time.utctDay now)
                  },
            cookieIsSecure = Servant.Auth.Server.NotSecure,
            cookieSameSite = SameSiteStrict,
            cookieXsrfSetting =
              Just
                XsrfCookieSettings
                  { xsrfCookieName = "XSRF-TOKEN",
                    xsrfHeaderName = "X-XSRF-TOKEN",
                    xsrfCookiePath = Just "/",
                    xsrfExcludeGet = False
                  }
          }
      ctx = cookieSettings :. jwtCfg :. EmptyContext
  putText $ "serving to port " <> show port
  run port $ serveWithContext (Proxy @AuthAPI) ctx (server cookieSettings jwtCfg)
  where
    port :: Int
    port = 3000
