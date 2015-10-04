module Yesod.Soap.Types
    ( SoapEnvelope (..)
    , SoapHeader (..)
    , SoapBody (..)
    , SoapFault (..)
    , FaultBody (..)
    , WsCoordinationContext (..)
    , WsRegistrationService (..)
    , readEnvelope
    , writeEnvelope
    ) where

import Text.XML.HXT.Arrow.Pickle

-- Soap Envelope definition

data SoapEnvelope m = SoapEnvelope {
    seHeader :: Maybe SoapHeader,
    seBody :: SoapBody m
} deriving (Show)

data SoapHeader = SoapHeader {
    -- WS-Coordination context
    shWsCoord :: Maybe WsCoordinationContext
} deriving (Show)

data SoapBody m = SoapBody {
    sbBody :: m
} deriving (Show)

type SoapFault = SoapBody FaultBody

data FaultBody = Fault {
    faultCode :: String,
    faultString :: String,
    faultActor :: Maybe String,
    faultDetail :: Maybe String
} deriving (Show)

data WsCoordinationContext = WsCoordinationContext {
    wcIdentifier :: String,
    wcExpires :: Integer,
    wcCoordinationType :: String,
    wcRegistrationService :: WsRegistrationService
} deriving (Show)

data WsRegistrationService = WsRegistrationService {
    wrsAddress :: String
} deriving (Show)

readEnvelope :: String -> SoapEnvelope m
readEnvelope = undefined

writeEnvelope :: SoapEnvelope m -> String
writeEnvelope = undefined

-- Xml picklers

instance XmlPickler m => XmlPickler (SoapEnvelope m) where
    xpickle = xpElem "Envelope" $
            xpAddFixedAttr "xmlns:s" "http://www.w3.org/2001/12/soap-envelope" $
            xpAddFixedAttr "s:encodingStyle" "http://www.w3.org/2001/12/soap-encoding" $
            xpWrap ( \ (h, b) -> SoapEnvelope h b
                , \ se -> (seHeader se, seBody se)
                ) $
            xpPair (xpOption (xpickle)) xpickle

instance XmlPickler SoapHeader where
    xpickle = xpElem "Header" $
            xpWrap ( \wscoord -> SoapHeader wscoord,
                \sh -> shWsCoord sh) $
            xpOption xpickle

instance XmlPickler WsCoordinationContext where
    xpickle = xpElem "CoordinationContext" $
            xpWrap ( \(i, e, c, rs) -> WsCoordinationContext i e c rs,
                \wscc -> (wcIdentifier wscc, wcExpires wscc, 
                    wcCoordinationType wscc, wcRegistrationService wscc)
                ) $
            xp4Tuple (xpElem "Identifier" xpText)
                     (xpElem "Expires" xpPrim)
                     (xpElem "CoordinationType" xpText)
                     (xpickle)

instance XmlPickler WsRegistrationService where
    xpickle = xpElem "RegistrationService" $
            xpWrap (\s -> WsRegistrationService s,
                \s -> wrsAddress s) $
            xpElem "Address" xpText

instance XmlPickler m => XmlPickler (SoapBody m) where
    xpickle = xpElem "Body" $
            xpWrap ( \body -> SoapBody body,
                \sb -> sbBody sb
                ) $
            xpickle
