{-# LANGUAGE NoImplicitPrelude #-}

module Util where

import           RIO
import           RIO.Text (pack)

-- | Helper function to pack a show-able value into text.
showText :: Show a => a -> Text
showText = pack . show
