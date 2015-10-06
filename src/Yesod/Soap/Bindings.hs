{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Soap.Bindings
    ( SoapBinding
    , Binding (..)
    ) where

import Control.Arrow((&&&))
import Text.XML.HXT.Core

import Yesod.Soap.Common

class Binding binding operation input output fault where
    xpickleBinding :: PU binding
    xpickleOperation :: PU operation
    xpickleInput :: PU input
    xpickleOutput :: PU output
    xpickleFault :: PU fault

data SoapBinding = SoapBinding {
    sbBinding :: SoapBindingElement,
    sbOperation :: SoapOperation,
    sbBody :: SoapBody
}deriving (Show)

data BindingStyle = Rpc | Document deriving Show

data SoapBindingElement = SoapBindingElement {
     sbeStyle :: Maybe BindingStyle,
     sbeTransport :: String
} deriving (Show)

data SoapOperation = SoapOperation {
    soAction :: Maybe String,
    soStyle :: Maybe BindingStyle
} deriving (Show)

data SoapBody = SoapBody {
    sbEncodingStyle :: Maybe String,
    sbNameSpace :: Maybe String,
    sbUse :: Maybe String
} deriving (Show)

-- Binding for Soap

instance Binding SoapBinding where
    xpickleBinding = undefined
    xpickleOperation = undefined
    xpickleInput = undefined
    xpickleOutput = undefined
    xpickleFault = undefined

-- XmlPicklers


instance XmlPickler SoapBody where
    xpickle = xpElemWsdlSoap "body" $
        xpWrap ( uncurry3 SoapBody,
            \s -> (sbEncodingStyle s, sbNameSpace s, sbUse s)
            ) $
        xpTriple (xpAttrImplied "encodingStyle" xpText)
                 (xpAttrImplied "namespace" xpText)
                 (xpAttrImplied "use" xpText)

-- should be undefined to prevent uncontrolled serialization

instance XmlPickler SoapBinding where
    xpickle = undefined