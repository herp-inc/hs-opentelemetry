{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenTelemetry.Instrumentation.Yesod (
  -- * Middleware functionality
  openTelemetryYesodMiddleware,
  RouteRenderer (..),
  mkRouteToRenderer,
  mkRouteToPattern,
  YesodOpenTelemetryTrace (..),
  getHandlerSpan,
  getHandlerSpan',
  spanKey,

  -- * Utilities
  rheSiteL,
  handlerEnvL,
) where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vault.Lazy as V
import Language.Haskell.TH
import Lens.Micro
import Network.Wai (Request (vault), requestHeaders)
import qualified OpenTelemetry.Context as Context
import OpenTelemetry.Context.ThreadLocal
import OpenTelemetry.Trace.Core hiding (getTracer, inSpan, inSpan', inSpan'')
import qualified OpenTelemetry.Trace.Monad as M
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception
import Yesod.Core
import Yesod.Core.Types
import Yesod.Routes.TH.Types


handlerEnvL :: Lens' (HandlerData child site) (RunHandlerEnv child site)
handlerEnvL = lens handlerEnv (\h e -> h {handlerEnv = e})
{-# INLINE handlerEnvL #-}


rheSiteL :: Lens' (RunHandlerEnv child site) site
rheSiteL = lens rheSite (\rhe new -> rhe {rheSite = new})
{-# INLINE rheSiteL #-}


class YesodOpenTelemetryTrace site where
  getTracerProvider :: site -> TracerProvider
  getTracer :: (MonadHandler m, HandlerSite m ~ site) => m Tracer
  default getTracer :: (MonadHandler m, HandlerSite m ~ site) => m Tracer
  getTracer =
    liftHandler $
      HandlerFor $ \hdata -> do
        let site = rheSite $ handlerEnv hdata
            tracerProvider = getTracerProvider site
        pure $ makeTracer tracerProvider "hs-opentelemetry-instrumentation-yesod" tracerOptions


instance (YesodOpenTelemetryTrace site) => M.MonadTracer (HandlerFor site) where
  getTracer = getTracer


{- | Template Haskell to generate a function named routeToRendererFunction.

 For a route like HomeR, this function returns "HomeR".

 For routes with parents, this function returns e.g. "FooR.BarR.BazR".

 See examples/yesod-minimal of hs-opentelemetry repository for usage.
-}
mkRouteToRenderer ::
  -- | Yesod site type
  Name ->
  -- | map from subsites type names to their @routeToRenderer@ Template Haskell expressions
  Map String ExpQ ->
  -- | route
  [ResourceTree String] ->
  Q [Dec]
mkRouteToRenderer appName subrendererExps ress = do
  let fnName = mkName "routeToRenderer"
  clauses <- mconcat <$> traverse (goTree id []) ress
  sequence
    [ sigD fnName [t|Route $(conT appName) -> Text|]
    , funD fnName clauses
    ]
  where
    goTree :: (Q Pat -> Q Pat) -> [String] -> ResourceTree String -> Q [Q Clause]
    goTree front names (ResourceLeaf res) = pure [goRes front names res]
    goTree front names (ResourceParent name _check pieces trees) =
      mconcat <$> traverse (goTree front' newNames) trees
      where
        ignored = (replicate toIgnore wildP ++) . pure
        toIgnore = length $ filter isDynamic pieces
        isDynamic Dynamic {} = True
        isDynamic Static {} = False
        front' = front . conP (mkName name) . ignored
        newNames = names <> [name]

    goRes :: (Q Pat -> Q Pat) -> [String] -> Resource String -> Q Clause
    goRes front names Resource {..} =
      case resourceDispatch of
        Methods {} ->
          clause
            [front $ recP (mkName resourceName) []]
            (normalB [|T.pack $ intercalate "." $ names <> [resourceName]|])
            []
        Subsite {..} -> do
          case M.lookup subsiteType subrendererExps of
            Just subrendererExp -> do
              subsiteVar <- newName "subsite"
              clause
                [conP (mkName resourceName) [varP subsiteVar]]
                (normalB [|resourceName <> "." <> $(subrendererExp) $(varE subsiteVar)|])
                []
            Nothing -> fail $ "mkRouteToRenderer: not found: " ++ subsiteType


{- | Template Haskell to generate a function named @routeToPattern@.

 See examples/yesod-minimal of hs-opentelemetry repository for usage.
-}
mkRouteToPattern ::
  -- | Yesod site type
  Name ->
  -- | map from subsites type names to their @routeToRenderer@ Template Haskell expressions
  Map String ExpQ ->
  -- | route
  [ResourceTree String] ->
  Q [Dec]
mkRouteToPattern appName subpatternExps ress = do
  let fnName = mkName "routeToPattern"
  sequence
    [ sigD fnName [t|Route $(conT appName) -> Text|]
    , funD fnName $ mkClause <$> flatten ress
    ]
  where
    isDynamic Dynamic {} = True
    isDynamic Static {} = False
    parentPieceWrapper (parentName, pieces) nestedPat =
      conP (mkName parentName) $
        mconcat
          [ replicate (length $ filter isDynamic pieces) wildP
          , [nestedPat]
          ]
    mkClause fr@FlatResource {..} = do
      let basePattern = renderPattern fr
      case frDispatch of
        Methods {} ->
          clause
            [foldr parentPieceWrapper (recP (mkName frName) []) frParentPieces]
            (normalB $ appE [|T.pack|] [|basePattern|])
            []
        Subsite {..} ->
          case M.lookup subsiteType subpatternExps of
            Just subpatternExp -> do
              subsiteVar <- newName "subsite"
              clause
                [foldr parentPieceWrapper (conP (mkName frName) [varP subsiteVar]) frParentPieces]
                (normalB [|basePattern <> $(subpatternExp) $(varE subsiteVar)|])
                []
            Nothing -> fail $ "mkRouteToPattern: not found: " ++ subsiteType


renderPattern :: FlatResource String -> String
renderPattern FlatResource {..} =
  concat $
    concat
      [ ["!" | not frCheck]
      , case formattedParentPieces <> concatMap routePortionSection frPieces of
          [] -> ["/"]
          pieces -> pieces
      , case frDispatch of
          Methods {..} -> case methodsMulti of
            Nothing -> []
            Just t -> ["/+", t]
          Subsite {} -> []
      ]
  where
    routePortionSection :: Piece String -> [String]
    routePortionSection (Static t) = ["/", t]
    routePortionSection (Dynamic t) = ["/#{", t, "}"]

    formattedParentPieces :: [String]
    formattedParentPieces = do
      (_parentName, pieces) <- frParentPieces
      piece <- pieces
      routePortionSection piece


data RouteRenderer site = RouteRenderer
  { nameRender :: Route site -> T.Text
  -- ^ give @routeToRenderer@ to
  , pathRender :: Route site -> T.Text
  -- ^ give @routeToPattern@ to
  }


openTelemetryYesodMiddleware ::
  (YesodOpenTelemetryTrace site, ToTypedContent res) =>
  RouteRenderer site ->
  HandlerFor site res ->
  HandlerFor site res
openTelemetryYesodMiddleware rr (HandlerFor doResponse) = do
  -- tracer <- OpenTelemetry.Trace.Monad.getTracer
  req <- waiRequest
  mspan <- Context.lookupSpan <$> getContext
  mr <- getCurrentRoute
  let sharedAttributes =
        catMaybes
          [ do
              r <- mr
              pure ("http.route", toAttribute $ pathRender rr r)
          , do
              ff <- lookup "X-Forwarded-For" $ requestHeaders req
              pure ("http.client_ip", toAttribute $ T.decodeUtf8 ff)
          ]
      args =
        defaultSpanArguments
          { kind = maybe Server (const Internal) mspan
          , attributes = sharedAttributes
          }
  mapM_ (`addAttributes` sharedAttributes) mspan
  eResult <- M.inSpan' (maybe "yesod.handler.notFound" (\r -> "yesod.handler." <> nameRender rr r) mr) args $ \s -> do
    catch
      ( HandlerFor $ \hdata@HandlerData {handlerRequest = hReq@YesodRequest {reqWaiRequest = waiReq}} -> do
          Right <$> doResponse (hdata {handlerRequest = hReq {reqWaiRequest = waiReq {vault = V.insert spanKey s $ vault waiReq}}})
      )
      $ \e -> do
        -- We want to mark the span as an error if it's an InternalError,
        -- the other HCError values are 4xx status codes which don't
        -- really count as a server error in OpenTelemetry spec parlance.
        case e of
          HCError (InternalError _) -> throwIO e
          _ -> pure (Left (e :: HandlerContents))
  case eResult of
    Left hc -> throwIO hc
    Right normal -> pure normal


spanKey :: V.Key Span
spanKey = unsafePerformIO V.newKey
{-# NOINLINE spanKey #-}


getHandlerSpan :: (MonadHandler m) => m (Maybe Span)
getHandlerSpan = liftHandler $ HandlerFor $ pure . V.lookup spanKey . vault . reqWaiRequest . handlerRequest


-- | When without Open Telemetry middleware, this fails.
getHandlerSpan' :: (MonadHandler m) => m Span
getHandlerSpan' = liftHandler $ HandlerFor $ pure . fromJust . V.lookup spanKey . vault . reqWaiRequest . handlerRequest
