module Yesod.Soap.Bindings
    ( SoapBinding
    , Binding (..)
    ) where

import Control.Arrow((&&&))
import Text.XML.HXT.Core

class Binding m where
    xpickleInput :: PU m
    xpickleOutput :: PU m
    xpickleFault :: PU m

data SoapBinding = SoapBinding {
    
} deriving (Show)

