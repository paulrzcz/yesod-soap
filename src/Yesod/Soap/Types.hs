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

import Data.Text (Text)
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
    faultCode :: Text,
    faultString :: Text,
    faultActor :: Maybe Text,
    faultDetail :: Maybe Text
} deriving (Show)

data WsCoordinationContext = WsCoordinationContext {
    wcIdentifier :: Text,
    wcExpires :: Integer,
    wcCoordinationType :: Text,
    wcRegistrationService :: WsRegistrationService
} deriving (Show)

data WsRegistrationService = WsRegistrationService {
    wrsAddress :: Text
} deriving (Show)

readEnvelope :: Text -> SoapEnvelope m
readEnvelope = undefined

writeEnvelope :: SoapEnvelope m -> Text
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
    xpickle = undefined

instance XmlPickler m => XmlPickler (SoapBody m) where
    xpickle = undefined