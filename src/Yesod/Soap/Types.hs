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
    , writeConfig
    , readConfig
    ) where

import Yesod.Soap.Common
import Control.Arrow((&&&))
import Text.XML.HXT.Core

-- Soap Envelope definition

type NsDeclaration = (String, String)

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

writeConfig :: SysConfigList
writeConfig = [
    withIndent yes
  , withValidate yes]

readConfig :: SysConfigList
readConfig = [
      withRemoveWS yes
    , withValidate yes
    , withCheckNamespaces yes]

readEnvelope :: XmlPickler m => String -> IO (Either String (SoapEnvelope m))
readEnvelope str = do 
    xs <- runX $ readString readConfig str 
    case xs of
        [] -> return (Left "No SOAP envelope found")
        [x] -> return $ unpickleDoc' xpSoapEnvelope x
        _ -> return (Left "Too many xml trees in input")

writeEnvelope :: XmlPickler m => SoapEnvelope m -> IO String
writeEnvelope envelope = do
    let xmlTree = pickleDoc xpSoapEnvelope envelope
    [str] <- runX (
        constA xmlTree
        >>>
        writeDocumentToString writeConfig
        )
    return str

-- Xml picklers

xpSoapEnvelope :: XmlPickler m => PU (SoapEnvelope m)
xpSoapEnvelope = xpElemSoap "Envelope" $
            xpAddNSDecl soapPrefix nsSoap $
            xpFilterAttr (hasName "encodingStyle") $
--            xpAddFixedAttr "s:encodingStyle" "http://www.w3.org/2001/12/soap-encoding" $
            xpWrap ( uncurry SoapEnvelope
                , seHeader &&& seBody
                ) $
            xpPair (xpOption xpickle) xpickle

instance XmlPickler m => XmlPickler (SoapEnvelope m) where
    xpickle = xpSoapEnvelope

instance XmlPickler SoapHeader where
    xpickle = xpElemSoap "Header" $
            xpWrap ( SoapHeader,
                shWsCoord) $
            xpOption xpickle

instance XmlPickler WsCoordinationContext where
    xpickle = xpElemCoor "CoordinationContext" $
            xpAddNSDecl addrPrefix nsAddr $
            xpAddNSDecl coorPrefix nsCoor $ 
            xpWrap ( \(i, e, c, rs) -> WsCoordinationContext i e c rs,
                \wscc -> (wcIdentifier wscc, wcExpires wscc, 
                    wcCoordinationType wscc, wcRegistrationService wscc)
                ) $
            xp4Tuple (xpElemCoor "Identifier" xpText)
                     (xpElemCoor "Expires" xpPrim)
                     (xpElemCoor "CoordinationType" xpText)
                     xpickle

instance XmlPickler WsRegistrationService where
    xpickle = xpElemCoor "RegistrationService" $
            xpWrap (WsRegistrationService,
                wrsAddress) $
            xpElemAddr "Address" xpText

instance XmlPickler m => XmlPickler (SoapBody m) where
    xpickle = xpElemSoap "Body" $
            xpWrap (SoapBody ,
                sbBody
                ) xpickle
