{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App.Docs (main) where

import App.ApiAuth
import App.Types
import Data.Aeson
import Data.Swagger
import Protolude
import Servant.Auth.Docs ()
import Servant.Auth.Swagger ()
import Servant.Docs hiding (API)
import Servant.Swagger

deriving instance ToSchema User

deriving instance ToSchema Login

instance ToSchema RootPage where
  declareNamedSchema _ = declareNamedSchema (Proxy @Any)

main :: IO ()
main = do
  writeFile "docs/servant.md" . toS . markdown . docs $ Proxy @AuthAPI
  writeFile "docs/swagger.json" . toS . encode $ toSwagger (Proxy @AuthAPI)
