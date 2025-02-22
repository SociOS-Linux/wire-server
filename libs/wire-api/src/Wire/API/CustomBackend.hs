{-# LANGUAGE StrictData #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.CustomBackend
  ( CustomBackend (..),

    -- * Swagger
    modelCustomBackend,
  )
where

import Data.Aeson
import Data.Json.Util ((#))
import Data.Misc (HttpsUrl)
import qualified Data.Swagger.Build.Api as Doc
import Imports
import Wire.API.Arbitrary (Arbitrary, GenericUniform (..))

data CustomBackend = CustomBackend
  { backendConfigJsonUrl :: HttpsUrl,
    backendWebappWelcomeUrl :: HttpsUrl
  }
  deriving stock (Eq, Show, Generic)
  deriving (Arbitrary) via (GenericUniform CustomBackend)

modelCustomBackend :: Doc.Model
modelCustomBackend = Doc.defineModel "CustomBackend" $ do
  Doc.description "Description of a custom backend"
  Doc.property "config_json_url" Doc.string' $
    Doc.description "the location of the custom backend's config.json file"
  Doc.property "webapp_welcome_url" Doc.string' $
    Doc.description "the location of the custom webapp"

instance ToJSON CustomBackend where
  toJSON j =
    object $
      "config_json_url" .= backendConfigJsonUrl j
        # "webapp_welcome_url" .= backendWebappWelcomeUrl j
        # []

instance FromJSON CustomBackend where
  parseJSON = withObject "CustomBackend" $ \o ->
    CustomBackend
      <$> o .: "config_json_url"
      <*> o .: "webapp_welcome_url"
