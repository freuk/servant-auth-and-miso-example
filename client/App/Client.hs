{-# LANGUAGE TypeApplications #-}

module App.Client (main) where

import App.Api
import App.MisoProtolude ()
import App.Types
import App.XhrClient
import Control.Lens
import Data.Generics.Labels ()
import qualified Data.Sequence as Seq
import qualified Language.Javascript.JSaddle as JSaddle
import Miso
import Miso.String
import qualified Network.HTTP.Types as NHT
import Protolude
import Servant.API
import Web.Cookie
import Prelude (String)

#ifdef __GHCJS__
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Foreign.Callback
#endif

#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp _ = panic "no jsaddle harness"
#else
runApp :: IO () -> IO ()
runApp app = app
#endif

data Model = Model
  { loggedIn :: Bool,
    usernameField :: MisoString,
    passwordField :: MisoString,
    passwordAgain :: MisoString,
    rollDieMessage :: MisoString
  }
  deriving (Eq, Show, Generic)

initialModel = Model False "" "" "" ""

data Result = PlaceholderForHttpErrorOrUser deriving (Show)

data Result'' = PlaceholderForHttpErrorOrInt deriving (Show)

data Msg
  = CheckLoggedInStatus
  | CheckLoggedInStatusResponse (Either Text User)
  | -- update model (login form)
    Username MisoString
  | Password MisoString
  | PasswordAgain MisoString
  | -- request a login with the data from the form
    LoginRequest
  | LoginResponse
      ( Either
          Text
          ( Headers
              '[ Header "Set-Cookie" SetCookie,
                 Header "Set-Cookie" SetCookie
               ]
              NoContent
          )
      )
  | RollDieRequest
  | RollDieResponse (Either Text Int)
  | Logout
  | Error MisoString
  | UnauthorizedError
  | NullMsg

roll :: ClientM Int
loggedin :: ClientM User
roll :<|> loggedin = client (Proxy @Unprotected)

login ::
  Login ->
  ClientM
    ( Headers
        '[ Header "Set-Cookie" Web.Cookie.SetCookie,
           Header "Set-Cookie" Web.Cookie.SetCookie
         ]
        NoContent
    )
_rootPage :<|> login :<|> _static = client (Proxy @Public)

servantErrorToS ::
  forall a b.
  StringConv String a =>
  ClientM b ->
  Seq.Seq NHT.Header ->
  IO (Either a b)
servantErrorToS c hs =
  runClientM hs c >>= \case
    Right x -> return $ Right x
    Left x -> return . Left $ show x

-- Runs an xhr request for a servant client, potentially adding
-- an existing XSRF token to the request header if the cookie
-- exists.
runClientIO ::
  (Either Text b -> action) ->
  ClientM b ->
  Transition action model ()
runClientIO constructor c = scheduleIO $ do
  headerSearchResult <-
    JSaddle.eval
      @Text
      "document.cookie.match(new RegExp('XSRF-TOKEN=([^;]+)'));"
  requestHeaders <-
    JSaddle.valIsNull headerSearchResult >>= \case
      True -> return mempty
      False ->
        JSaddle.fromJSVal @[Text] headerSearchResult <&> \case
          Nothing -> mempty
          Just hlist ->
            hlist ^? ix 1 & \case
              Nothing -> mempty
              Just h -> pure ("X-XSRF-TOKEN", toS h)
  constructor <$> liftIO (servantErrorToS c requestHeaders)

-- | Entry point for a miso application
main :: IO ()
main =
  runApp . startApp $
    App
      { initialAction = CheckLoggedInStatus,
        model = initialModel,
        update = fromTransition . updateModel,
        view = viewModel,
        events = defaultEvents,
        subs = [],
        mountPoint = Nothing,
        logLevel = Off
      }

updateModel :: Msg -> Transition Msg Model ()
updateModel = \case
  (LoginResponse (Left _)) ->
    scheduleIO_ $ consoleLog "LoginResponse: Authentication Error"
  (LoginResponse (Right _)) -> do
    #loggedIn .= True
    #passwordField .= ""
    #passwordAgain .= ""
  LoginRequest -> do
    us <- toS <$> use #usernameField
    pw <- toS <$> use #passwordField
    runClientIO LoginResponse (login $ Login us pw)
  CheckLoggedInStatus -> runClientIO CheckLoggedInStatusResponse loggedin
  CheckLoggedInStatusResponse (Left _) -> pass
  CheckLoggedInStatusResponse (Right (userEmail -> email)) -> do
    #loggedIn .= True
    #usernameField .= toS email
  (Username u) -> #usernameField .= u
  (Password p) -> #passwordField .= p
  PasswordAgain p -> #passwordAgain .= p
  RollDieRequest -> runClientIO RollDieResponse roll
  RollDieResponse (Left _) -> scheduleIO_ (consoleLog "Couldn't roll")
  RollDieResponse (Right x) ->
    #rollDieMessage .= ("Roll success! result: " <> show x)
  Logout -> do
    scheduleIO_
      ( do
          _ <-
            JSaddle.eval @Text
              "document.cookie = 'XSRF-TOKEN=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;';"
          _ <- JSaddle.eval @Text "window.location.href = '/';"
          (consoleLog "Logout")
      )
    #loggedIn .= False
  Error _ -> scheduleIO_ (consoleLog "Error")
  UnauthorizedError -> scheduleIO_ (consoleLog "UnauthorizedError")
  NullMsg -> pass

viewModel :: Model -> View Msg
viewModel m =
  let loginStatusTitle =
        [div_ [] [text (if loggedIn m then "You are currently logged in" else "Please log in")]]
      loginForm =
        if not (loggedIn m)
          then
            [ form_
                [onSubmit NullMsg]
                [ label_ [] [text "Username:"],
                  br_ [],
                  input_ [type_ "text", placeholder_ "Username", onInput Username],
                  br_ [],
                  label_ [] [text "Password:"],
                  br_ [],
                  input_ [type_ "password", placeholder_ "Password", onInput Password],
                  br_ [],
                  label_ [] [text "Password Again:"],
                  br_ [],
                  input_ [type_ "password", placeholder_ "Re-enter Password", onInput PasswordAgain],
                  br_ [],
                  button_ [onClick LoginRequest] [text "Login"]
                ],
              viewValidation m
            ]
          else []
      loginOnlyButton :: [View Msg]
      loginOnlyButton =
        [button_ [onClick RollDieRequest] [text "Roll Die (Require login)"]]
      rollDieMessage' =
        [ div_
            []
            [text (rollDieMessage m)]
          | rollDieMessage m /= ""
        ]
      logoutButton :: [View Msg]
      logoutButton =
        [button_ [onClick Logout] [text "Logout"]]
   in div_ [] (loginStatusTitle <> loginForm <> loginOnlyButton <> rollDieMessage' <> logoutButton)

viewValidation :: Model -> View Msg
viewValidation m =
  let (color, message) =
        if usernameField m /= ""
          && passwordField m /= ""
          && passwordField m == passwordAgain m
          then ("green", "OK")
          else ("red", "No fields may be empty and passwords must match.")
   in div_ [style_ [("color", color)]] [text message]
