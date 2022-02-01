module Main where

import Prelude

import Chanterelle.Internal.Logging (LogLevel(..), log)
import DApp.Support (mkEnv)
import Data.Either (Either(..))
import Data.Int (fromString) as Int
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
import Effect.Aff (attempt, error, launchAff_, throwError)
import Effect.Class (liftEffect)
import Node.HTTP as NH
import Node.Net.Socket as NNS
import Node.Process (lookupEnv)
import Node.Stream as NS
import Nodetrout (serve)
import Routes (appRoutes, routeHandlers)
import Types (runAppM)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = launchAff_ do
  run <- attempt do
    hostname <- liftEffect $ fromMaybe "0.0.0.0" <$> lookupEnv "SERVER_ADDRESS"
    portS <- liftEffect $ lookupEnv "SERVER_PORT"
    port <- case portS of
      Nothing -> pure 3000
      Just str -> case Int.fromString str of
        Nothing -> throwError <<< error $ "Couldn't parse " <> show portS <> " as a port number"
        Just portNumber -> pure portNumber
    env <- mkEnv
    server <- liftEffect $ NH.createServer <<< withLoggingMiddleware $ serve appRoutes routeHandlers (runAppM env) (log Error <<< show)
    liftEffect $ NH.listen server { hostname, port, backlog: Nothing } $ (log Info $ "Started listening on http://" <> hostname <> ":" <> show port)
  case run of
    Left err -> log Error $ "FATAL " <> show err
    _ -> pure unit

withLoggingMiddleware :: (NH.Request -> NH.Response -> Effect Unit) -> NH.Request -> NH.Response -> Effect Unit
withLoggingMiddleware runServer req res = do
  let method = NH.requestMethod req
      url = NH.requestURL req
      httpVersion =  (_.httpVersion <<< unsafeCoerce) req
      requestSocket = (_.socket <<< unsafeCoerce) req
      logRes level = do
        -- note that logging  statusCode/message is meaningless here, as Nodetrout uses explicit headers (i.e., calls writeHead)
        remoteAddr <- fromMaybe "<?>" <$> NNS.remoteAddress requestSocket
        remotePort <- maybe "<?>" show <$> NNS.remotePort requestSocket
        log level $ "served " <> remoteAddr <> ":" <> remotePort <> " \"" <> method <> " " <> url <> " HTTP/" <> httpVersion <> "\" "
  NS.onFinish (NH.responseAsStream res) $ logRes Info
  NS.onError (NH.responseAsStream res) $ \_ -> logRes Error
  runServer req res
