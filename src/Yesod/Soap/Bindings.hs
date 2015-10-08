{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Soap.Bindings
    ( SoapBinding (..)
    , SoapBindingElement (..)
    , SoapOperation (..)
    , WsdlSoapBody (..)
    , WsdlSoapFault (..)
    , Binding (..)
    ) where

import Data.Proxy
import Control.Arrow((&&&))
import Text.XML.HXT.Core

import Yesod.Soap.Common

class Binding m where
    type GBinding m
    type GOperation m
    type GInput m
    type GOutput m
    type GFault m
    xpickleBinding :: XmlPickler (GBinding m) => Proxy m -> PU (GBinding m)
    xpickleBinding _ = xpickle
    xpickleOperation :: XmlPickler (GOperation m) => Proxy m -> PU (GOperation m)
    xpickleOperation _ = xpickle
    xpickleInput :: XmlPickler (GInput m) => Proxy m -> PU (GInput m)
    xpickleInput _ = xpickle
    xpickleOutput :: XmlPickler (GOutput m) => Proxy m -> PU (GOutput m)
    xpickleOutput _ = xpickle
    xpickleFault :: XmlPickler (GFault m) => Proxy m -> PU (GFault m)
    xpickleFault _ = xpickle

instance Binding SoapBinding where
    type GBinding SoapBinding = SoapBindingElement
    type GOperation SoapBinding = SoapOperation
    type GInput SoapBinding = WsdlSoapBody
    type GOutput SoapBinding = WsdlSoapBody
    type GFault SoapBinding = WsdlSoapFault

data SoapBinding = SoapBinding {
    wsdlSbBinding :: SoapBindingElement,
    wsdlSbOperation :: SoapOperation,
    wsdlSbBody :: WsdlSoapBody
}deriving (Show)

data BindingStyle = Rpc | Document deriving (Show)

data SoapBindingElement = SoapBindingElement {
     sbeStyle :: Maybe BindingStyle,
     sbeTransport :: String
} deriving (Show)

data SoapOperation = SoapOperation {
    soAction :: Maybe String,
    soStyle :: Maybe BindingStyle
} deriving (Show)

data WsdlSoapBody = WsdlSoapBody {
    sbEncodingStyle :: Maybe String,
    sbNameSpace :: Maybe String,
    sbUse :: Maybe String
} deriving (Show)

data WsdlSoapFault = WsdlSoapFault {
    
} deriving (Show)

-- XmlPicklers


instance XmlPickler WsdlSoapBody where
    xpickle = xpElemWsdlSoap "body" $
        xpWrap ( uncurry3 WsdlSoapBody,
            \s -> (sbEncodingStyle s, sbNameSpace s, sbUse s)
            ) $
        xpTriple (xpAttrImplied "encodingStyle" xpText)
                 (xpAttrImplied "namespace" xpText)
                 (xpAttrImplied "use" xpText)

instance XmlPickler SoapBindingElement where
    xpickle = xpElemWsdlSoap "binding" $
        xpWrap (uncurry SoapBindingElement,
            sbeStyle &&& sbeTransport) $
        xpPair (xpAttrImplied "style" xpickle)
               (xpAttr "transport" xpText)

instance XmlPickler BindingStyle where
    xpickle = xpWrap (fromStr, toStr) xpText
        where
            toStr Rpc = "rpc"
            toStr Document = "document"
            fromStr "rpc" = Rpc
            fromStr "document" = Document

instance XmlPickler SoapOperation where
    xpickle = xpElemWsdlSoap "operation" $
        xpWrap (uncurry SoapOperation,
            soAction &&& soStyle) $
        xpPair (xpAttrImplied "soapAction" xpText)
               (xpAttrImplied "style" xpickle)

instance XmlPickler WsdlSoapFault where
    xpickle = undefined

-- should be undefined to prevent uncontrolled serialization

instance XmlPickler SoapBinding where
    xpickle = undefined