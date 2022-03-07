{-# LANGUAGE StandaloneKindSignatures #-}

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
module Wire.API.Error where

import Control.Lens (at, (%~), (.~), (?~))
import qualified Data.Aeson as A
import Data.Proxy
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as Text
import GHC.TypeLits
import Imports
import Numeric.Natural
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Servant
import Servant.Swagger
import Wire.API.Routes.MultiVerb

-- | Runtime representation of a statically-known error.
data DynError = DynError
  { eCode :: Natural,
    eLabel :: Text,
    eMessage :: Text
  }

-- | A statically-known error. This is meant to be used as a kind.
data StaticError = StaticError
  { seCode :: Nat,
    seLabel :: Symbol,
    seMessage :: Symbol
  }

-- | The singleton corresponding to 'StaticError'. This is hand-written,
-- because the singletons library has problems with promoted natural numbers.
data SStaticError e where
  SStaticError ::
    (KnownNat c, KnownSymbol l, KnownSymbol msg) =>
    Proxy c ->
    Proxy l ->
    Proxy msg ->
    SStaticError ('StaticError c l msg)
  deriving (A.FromJSON, A.ToJSON, S.ToSchema) via Schema (SStaticError e)

class KnownError (e :: StaticError) where
  seSing :: SStaticError e

instance (KnownNat c, KnownSymbol l, KnownSymbol msg) => KnownError ('StaticError c l msg) where
  seSing = SStaticError Proxy Proxy Proxy

dynError' :: SStaticError e -> DynError
dynError' (SStaticError c l msg) = mkDynError c l msg

mkDynError ::
  (KnownNat c, KnownSymbol l, KnownSymbol msg) =>
  Proxy c ->
  Proxy l ->
  Proxy msg ->
  DynError
mkDynError c l msg =
  DynError
    (toEnum (fromIntegral (natVal c)))
    (Text.pack (symbolVal l))
    (Text.pack (symbolVal msg))

dynError :: forall e. KnownError e => DynError
dynError = dynError' $ seSing @e

staticErrorSchema :: SStaticError e -> ValueSchema NamedSwaggerDoc (SStaticError e)
staticErrorSchema e@(SStaticError c l m) =
  objectWithDocModifier "Error" addExample $
    SStaticError
      <$> (c <$ (const code .= field "code" codeSchema))
      <*> (l <$ (const label .= field "label" labelSchema))
      <*> (m <$ (const message .= field "message" schema))
  where
    err = dynError' e
    label = eLabel err
    code = eCode err
    message = eMessage err

    addExample = S.schema . S.example ?~ A.toJSON e
    labelSchema :: ValueSchema SwaggerDoc Text
    labelSchema = unnamed $ enum @Text "Label" (element label label)
    codeSchema :: ValueSchema SwaggerDoc Natural
    codeSchema = unnamed $ enum @Natural "Status" (element code code)

instance KnownError e => ToSchema (SStaticError e) where
  schema = staticErrorSchema seSing

data DeclareError (e :: *)

instance (HasServer api ctx) => HasServer (DeclareError t :> api) ctx where
  type ServerT (DeclareError t :> api) m = ServerT api m

  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance
  (HasSwagger api, KnownError (MapError e)) =>
  HasSwagger (DeclareError e :> api)
  where
  toSwagger _ = addToSwagger @(MapError e) (toSwagger (Proxy @api))

type family DeclaredErrorEffects api :: EffectRow where
  DeclaredErrorEffects (DeclareError e :> api) = (ErrorS e ': DeclaredErrorEffects api)
  DeclaredErrorEffects (x :> api) = DeclaredErrorEffects api
  DeclaredErrorEffects api = '[]

addToSwagger :: forall e. KnownError e => S.Swagger -> S.Swagger
addToSwagger =
  S.allOperations . S.responses . S.responses . at (fromIntegral (eCode err))
    %~ Just . addRef
  where
    err = dynError @e

    addRef :: Maybe (S.Referenced S.Response) -> S.Referenced S.Response
    addRef Nothing = S.Inline resp
    addRef (Just (S.Inline resp1)) = S.Inline (combineResponseSwagger resp1 resp)
    addRef (Just r@(S.Ref _)) = r

    resp =
      mempty
        & S.description .~ desc
        & S.schema ?~ S.Inline (S.toSchema (Proxy @(SStaticError e)))

    desc = eMessage err <> " (label: `" <> eLabel err <> "`)"

class IsError e where
  type MapError (x :: e) :: StaticError

data ErrorS e m a where
  ThrowS :: Proxy e -> ErrorS e m a

throwS :: forall e r a. Member (ErrorS e) r => Sem r a
throwS = send (ThrowS (Proxy @e))

runErrorS ::
  forall e r a.
  (Member (Error DynError) r, KnownError (MapError e)) =>
  Sem (ErrorS e ': r) a ->
  Sem r a
runErrorS = interpret $ \case ThrowS _ -> throw (dynError @(MapError e))

--------------------------------------------------------------------------------
-- Example

data GalleyError
  = InvalidAction
  | InvalidTargetAccess

instance IsError GalleyError where
  type MapError 'InvalidAction = 'StaticError 400 "invalid-action" "Invalid action"
  type MapError 'InvalidTargetAccess = 'StaticError 403 "invalid-target-access" "Invalid target access"
