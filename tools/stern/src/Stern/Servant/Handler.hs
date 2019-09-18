module Stern.Servant.Handler where

import Imports hiding (head)

import Brig.Types.Intra
import Control.Exception (throwIO)
import Control.Lens (view)
import Control.Monad.Catch (catch)
import Control.Monad.Trans.Except
import Data.Aeson (Value)
import Data.Id
import Data.Proxy
import Data.String.Conversions (cs)
import "swagger2" Data.Swagger
import GHC.TypeLits (symbolVal)
import Network.Wai
import Servant.API.ContentTypes
import Servant.API.Generic
import Servant.Server
import Servant.Server.Generic
import Servant.Swagger
import Servant.Swagger.UI
import Stern.App hiding (Handler, runHandler, App)
import Stern.Intra as Intra
import Stern.Servant.Orphans ()
import Stern.Servant.Types

import qualified Data.Metrics.Middleware as Metrics


----------------------------------------------------------------------
-- middleware

middleware :: Env -> Middleware
middleware env innerapp req cont = if rootPrefix `isPrefixOf` pathInfo req
  then app env req cont
  else innerapp req cont
  where
    rootPrefix :: [Text]
    rootPrefix = [cs $ symbolVal (Proxy @RootPrefix)]


----------------------------------------------------------------------
-- custom handler type, natural transformation

-- | Like Stern.App.AppT, but with a servant handler inside.
type App = AppT IO

app :: Env -> Application
app env = serve
    (Proxy @(ToServant API AsApi))
    (hoistServer (Proxy @(ToServant API AsApi)) (appToServantHandler env) (genericServerT server))

appToServantHandler :: Env -> App a -> Handler a
appToServantHandler env (AppT m) = Handler . ioToExceptT $ m `runReaderT` env
  where
    ioToExceptT :: IO a -> ExceptT ServantErr IO a
    ioToExceptT action = ExceptT $ (Right <$> action) `catch` \(e :: ServantErr) -> pure (Left e)

servantHandlerToApp :: Handler a -> App a
servantHandlerToApp (Handler exc) = AppT . ReaderT . const . ioToExceptT $ exc
  where
    ioToExceptT :: ExceptT ServantErr IO a -> IO a
    ioToExceptT action = either throwIO pure =<< runExceptT action


----------------------------------------------------------------------
-- swagger docs

swaggerDoc :: Swagger
swaggerDoc = toSwagger (genericApi (Proxy :: Proxy API))


----------------------------------------------------------------------
-- handlers

server :: API (AsServerT App)
server = API
  { _apiSwaggerDoc         = hoistServer (Proxy @(SwaggerSchemaUI "api-docs" "swagger.json")) servantHandlerToApp
                             (swaggerSchemaUIServer swaggerDoc)
  , _apiInternalGetStatus  = apiInternalGetStatus
  , _apiInternalHeadStatus = apiInternalHeadStatus
  , _apiInternalMonitoring = apiInternalMonitoring
  , _apiSuspendUser        = apiSuspendUser
  , _apiUnsuspendUser      = apiUnsuspendUser
  }


apiInternalGetStatus :: Monad m => m NoContent
apiInternalGetStatus = pure NoContent

apiInternalHeadStatus :: Monad m => m NoContent
apiInternalHeadStatus = pure NoContent

apiInternalMonitoring :: (MonadIO m, MonadReader Env m) => m Value
apiInternalMonitoring = view metrics >>= Metrics.render


apiSuspendUser :: UserId -> MonadIntra m => m NoContent
apiSuspendUser uid = do
  Intra.putUserStatus Suspended uid
  pure NoContent

apiUnsuspendUser :: UserId -> MonadIntra m => m NoContent
apiUnsuspendUser uid = do
  Intra.putUserStatus Active uid
  pure NoContent
