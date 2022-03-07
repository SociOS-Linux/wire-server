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

module Wire.API.Routes.API where

import Data.Domain
import Data.Proxy
import Data.Swagger
import Imports
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Sing
import Polysemy.Internal.Union
import Servant hiding (Union)
import Servant.Swagger
import Unsafe.Coerce
import Wire.API.Error

-- | A Servant handler bundled with its Swagger documentation.
--
-- This is used to build Swagger documentation incrementally using information
-- that is not contained in the 'api' types. For example, documentation on
-- errors can be generated using the list of polysemy effects in the type of
-- the handler.
data API api = API
  { apiHandler :: ServerT api Handler,
    apiSwagger :: Swagger
  }

-- | Convert a polysemy handler to an 'API' value.
toAPI ::
  forall api r.
  ( ServerEffects r,
    ErrorSwagger r,
    HasSwagger api,
    HasServer api '[Domain]
  ) =>
  ServerT api (Sem r) ->
  API api
toAPI h =
  API
    (hoistServerWithDomain @api (toHandler @r) h)
    (errorSwagger @r (toSwagger (Proxy @api)))

-- | Combine APIs.
(<@>) :: API api1 -> API api2 -> API (api1 :<|> api2)
(<@>) (API h1 s1) (API h2 s2) =
  API
    (h1 :<|> h2)
    (s1 <> s2)

class ErrorSwagger (r :: EffectRow) where
  errorSwagger :: Swagger -> Swagger

instance ErrorSwagger '[] where
  errorSwagger = id

instance {-# OVERLAPPABLE #-} ErrorSwagger r => ErrorSwagger (eff ': r) where
  errorSwagger = errorSwagger @r

instance (ErrorSwagger r, KnownError (MapError e)) => ErrorSwagger (ErrorS e ': r) where
  errorSwagger = addToSwagger @(MapError e) . errorSwagger @r

-- Servant needs a context type argument here that contains *at least* the
-- context types required by all the HasServer instances. In reality, this should
-- not be necessary, because the contexts are only used by the @route@ functions,
-- but unfortunately the 'hoistServerWithContext' function is also part of the
-- 'HasServer' typeclass, even though it cannot possibly make use of its @context@
-- type argument.
hoistServerWithDomain ::
  forall api m n.
  HasServer api '[Domain] =>
  (forall x. m x -> n x) ->
  ServerT api m ->
  ServerT api n
hoistServerWithDomain = hoistServerWithContext (Proxy @api) (Proxy @'[Domain])

-- | Helper class to add effects at the end of a stack.
class Shift r where
  shiftUnion :: forall r0 m a. Proxy r0 -> Union r m a -> Union (Append r r0) m a

-- | Add any number of effects at the end of a stack.
shift :: forall r r0 a. Shift r => Sem r a -> Sem (Append r r0) a
shift = hoistSem $ hoist (shift @r @r0) . shiftUnion @r (Proxy @r0)

instance Shift '[] where
  shiftUnion _ = absurdU

instance Shift r => Shift (eff ': r) where
  shiftUnion r0 = either (weaken . shiftUnion @r r0) injWeaving . decomp

-- | An effect that can be interpreted intto the Handler monad.
class ServerEffect (eff :: Effect) where
  interpretServerEffect :: Member (Embed Handler) r => Sem (eff ': r) a -> Sem r a

class (Shift r, KnownList r) => ServerEffects (r :: [Effect]) where
  interpretServerEffects :: Member (Embed Handler) r1 => Sem (Append r r1) a -> Sem r1 a

instance ServerEffects '[] where
  interpretServerEffects = id

-- | A function @c => x@, where @c@ is a constraint.
newtype Constrained c x = Constrained (c => x)

-- | Convert dictionary to instance, CPS style. This uses 'unsafeCoerce' under
-- the hood to convert a constrained value @Member e r => x@ into a function
-- @ElemOf e r -> x@ taking the membership evidence explicitly as an argument.
withMember :: forall e r x. ElemOf e r -> (Member e r => x) -> x
withMember mem k = unsafeCoerce (Constrained @(Member e r) k) mem

instance (ServerEffects r, ServerEffect eff) => ServerEffects (eff ': r) where
  interpretServerEffects :: forall r1 a. Member (Embed Handler) r1 => Sem (Append (eff ': r) r1) a -> Sem r1 a
  interpretServerEffects h =
    withMember (extendMembershipLeft (singList @r) (membership @(Embed Handler) @r1)) $
      interpretServerEffects @r @r1 $
        interpretServerEffect @eff @(Append r r1) h

-- | Interpret all effects to 'Handler'.
toHandler :: forall r a. ServerEffects r => Sem r a -> Handler a
toHandler = runM . interpretServerEffects @r @'[Embed Handler] . shift @r @'[Embed Handler]
