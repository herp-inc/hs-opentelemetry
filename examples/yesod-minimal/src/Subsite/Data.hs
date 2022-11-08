{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Subsite.Data where

import Data.Text (Text)
import OpenTelemetry.Instrumentation.Yesod
import Yesod.Core


data Subsite = Subsite


$( do
    let routes =
          [parseRoutes|
          / SubHomeR GET
          /foo FooR GET
        |]
    concat
      <$> sequence
        [ mkRouteToRenderer ''Subsite mempty routes
        , mkRouteToPattern ''Subsite routes
        , mkYesodSubData "Subsite" routes
        ]
 )


getSubHomeR :: SubHandlerFor Subsite master Text
getSubHomeR = pure "SubHome"


getFooR :: SubHandlerFor Subsite master Text
getFooR = pure "Foo"
