module Routes
  ( module ChainInfo
  , module NullRoute
  , AppRoutes
  , appRoutes
  , routeHandlers
  ) where

import Routes.ChainInfo (ChainInfo(..), ChainInfoRoute, chainInfoRoute) as ChainInfo
import Routes.NullRoute (NullRoute, NullRouteRes(..), nullRoute) as NullRoute
import Routes.Relay (RelayRoute, relayRoute) as RelayRoute
import Type.Proxy (Proxy(..))
import Type.Trout (type (:<|>))

type AppRoutes = NullRoute.NullRoute :<|> ChainInfo.ChainInfoRoute :<|> RelayRoute.RelayRoute

appRoutes :: Proxy AppRoutes
appRoutes = Proxy

routeHandlers :: _
routeHandlers = { chain: ChainInfo.chainInfoRoute, null: NullRoute.nullRoute, relay: RelayRoute.relayRoute }