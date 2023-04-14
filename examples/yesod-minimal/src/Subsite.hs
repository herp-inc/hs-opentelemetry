{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Subsite (
  module Subsite.Data,
  module Subsite,
) where

import Subsite.Data
import Yesod.Core


instance (Yesod master) => YesodSubDispatch Subsite master where
  yesodSubDispatch = $(mkYesodSubDispatch resourcesSubsite)
