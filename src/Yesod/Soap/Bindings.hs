{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Soap.Bindings
    ( SoapBinding
    , SoapBindingElement 
    , SoapOperation 
    , SoapBody 
    , SoapFault
    ) where

import Control.Arrow((&&&))
import Text.XML.HXT.Core

import Yesod.Soap.Common

data SoapBinding = SoapBinding {
    sbBinding :: SoapBindingElement,
    sbOperation :: SoapOperation,
    sbBody :: SoapBody
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

data SoapBody = SoapBody {
    sbEncodingStyle :: Maybe String,
    sbNameSpace :: Maybe String,
    sbUse :: Maybe String
} deriving (Show)

data SoapFault = SoapFault {
    
} deriving (Show)

-- XmlPicklers


instance XmlPickler SoapBody where
    xpickle = xpElemWsdlSoap "body" $
        xpWrap ( uncurry3 SoapBody,
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

instance XmlPickler SoapFault where
    xpickle = undefined

-- should be undefined to prevent uncontrolled serialization

instance XmlPickler SoapBinding where
    xpickle = undefined