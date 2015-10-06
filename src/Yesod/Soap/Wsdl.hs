module Yesod.Soap.Wsdl
    ( Wsdl (..)
    , WsdlMessage (..)
    , WsdlMessagePart (..)
    , WsdlPortType (..)
    , WsdlPortOperation (..)
    , WsdlOperation (..) 
    , WsdlParam (..)
    , WsdlFault (..)
    , WsdlBinding (..)
    , WsdlBindingOperation (..)
    , WsdlBindingMessage (..)
    , WsdlBindingFault (..)
    , WsdlService (..)
    , WsdlPort (..)
    , QName (..)
    , readWsdl
    , writeWsdl
    ) where

import Control.Arrow((&&&))
import Yesod.Soap.Common
import Yesod.Soap.Types(writeConfig, readConfig)
import Text.XML.HXT.Core

data Wsdl m = Wsdl { -- definitions
    wsdlNameSpace :: Maybe String,
    wsdlName :: Maybe String,
    wsdlMessages :: [WsdlMessage],
    wsdlPortTypes :: [WsdlPortType],
    wsdlBindings  :: [WsdlBinding m],
    wsdlServices  :: [WsdlService]
} deriving (Show)

data WsdlMessage = WsdlMessage {
    msgName :: String,
    msgParts :: [WsdlMessagePart]
} deriving (Show)

data WsdlMessagePart = WsdlMessagePart {
    partName :: String,
    partElement :: Maybe QName,
    partType :: Maybe QName
} deriving (Show)

data WsdlPortType = WsdlPortType {
    portTypeName :: String,
    portTypeOperations :: [WsdlPortOperation]
} deriving (Show)

data WsdlPortOperation = WsdlPortOperation {
    operationName :: String,
    operationParameterOrder :: Maybe String,
    operationDesc :: [WsdlOperation]
} deriving (Show)

data WsdlOperation = 
    RequestResponse {
        opInput :: WsdlParam,
        opOutput :: WsdlParam,
        opFault :: [WsdlFault]
    } 
    | OneWayOperation {
        opInput :: WsdlParam
    }
    | SolicitResponse {
        opOutput :: WsdlParam,
        opInput :: WsdlParam,
        opFault :: [WsdlFault]
    }
    | NotificationOperation {
        opOutput :: WsdlParam
    } deriving (Show)

data WsdlParam = WsdlParam {
    paramName :: Maybe String,
    paramMessage :: QName
} deriving (Show)

data WsdlFault = WsdlFault {
    faultName :: Maybe String,
    faultMessage :: QName    
} deriving (Show)

data WsdlBinding m = WsdlBinding {
    bindingName :: String,
    bindingType :: QName,
    bindingOps  :: [WsdlBindingOperation m]
} deriving (Show)

data WsdlBindingOperation m = WsdlBindingOperation {
    bindingInputs :: [WsdlBindingMessage m],
    bindingOutputs :: [WsdlBindingMessage m],
    bindingFaults :: [WsdlBindingFault]    
} deriving (Show)

data WsdlBindingMessage m = WsdlBindingMessage {
    bindingMsgName :: Maybe String,
    bindingMsg :: m
} deriving (Show)

data WsdlBindingFault = WsdlBindingFault {
    bindingFaultName :: String
} deriving (Show)

data WsdlService = WsdlService {
    serviceName :: String,
    servicePorts :: [WsdlPort]
} deriving (Show)

data WsdlPort = WsdlPort {
    portName :: String,
    portBinding :: QName
} deriving (Show)

-- read / write section

readWsdl :: XmlPickler m => String -> IO (Either String (Wsdl m))
readWsdl str = do 
    xs <- runX $ readString readConfig str 
    case xs of
        [] -> return (Left "No WSDL found")
        [x] -> return $ unpickleDoc' xpWsdl x
        _ -> return (Left "Too many xml trees in input")

writeWsdl :: XmlPickler m => Wsdl m -> IO String
writeWsdl envelope = do
    let xmlTree = pickleDoc xpWsdl envelope
    [str] <- runX (
        constA xmlTree
        >>>
        writeDocumentToString writeConfig
        )
    return str

-- XmlPicker

xpWsdl :: XmlPickler m => PU (Wsdl m)
xpWsdl = xpElemWsdl "definitions" $
    xpAddNSDecl wsdlPrefix nsWsdl $
    xpWrap (\ (ns, n, m, p, b, s) -> Wsdl ns n m p b s,
        \w -> (wsdlNameSpace w, wsdlName w,
        wsdlMessages w, wsdlPortTypes w,
        wsdlBindings w, wsdlServices w)
        ) $
    xp6Tuple (xpOption (xpAttr "targetNamespace" xpText))
             (xpOption (xpAttr "name" xpText))
             (xpList xpickle)
             (xpList xpickle)
             (xpList xpickle)
             (xpList xpickle)

instance XmlPickler QName where
    xpickle = xpWrap (mkName, qualifiedName) xpText

instance XmlPickler m => XmlPickler (Wsdl m) where
    xpickle = xpWsdl

instance XmlPickler WsdlMessage where
    xpickle = xpElemWsdl "message" $
        xpWrap (uncurry WsdlMessage,
            msgName &&& msgParts) $
        xpPair (xpAttr "name" xpText) (xpList xpickle)

instance XmlPickler WsdlMessagePart where
    xpickle = xpElemWsdl "part" $
        xpWrap (uncurry3 WsdlMessagePart,
            \w -> (partName w, partElement w, partType w)
            ) $
        xpTriple (xpAttr "name" xpText)
                 (xpOption xpickle)
                 (xpOption xpickle)

instance XmlPickler WsdlPortType where
    xpickle = xpElemWsdl "portType" $
        xpWrap (uncurry WsdlPortType,
            portTypeName &&& portTypeOperations) $
        xpPair (xpAttr "name" xpText)
               (xpList xpickle) 

instance XmlPickler WsdlPortOperation where
    xpickle = xpElemWsdl "operation" $
        xpWrap (uncurry3 WsdlPortOperation,
            \w -> (operationName w, operationParameterOrder w, operationDesc w)) $
        xpTriple (xpAttr "name" xpText)
                 (xpOption (xpAttr "parameterOrder" xpText))
                 (xpList xpickle)

instance XmlPickler WsdlOperation where
    xpickle = xpAlt tag ps
        where
            tag (RequestResponse {})        = 0
            tag (OneWayOperation {})        = 1
            tag (SolicitResponse {})        = 2
            tag (NotificationOperation {})  = 3
            ps = [ 
                xpWrap (
                    uncurry3 RequestResponse,
                    \r -> (opInput r, opOutput r, opFault r)
                ) $ xpTriple (xpElemWsdl "input" xpickle) 
                             (xpElemWsdl "output" xpickle) 
                             (xpList xpickle),
                xpWrap (
                    OneWayOperation,
                    opInput) xpickle,
                xpWrap (
                    uncurry3 SolicitResponse,
                    \sr -> (opOutput sr, opInput sr, opFault sr)
                ) $ xpTriple (xpElemWsdl "output" xpickle) 
                             (xpElemWsdl "input" xpickle) 
                             (xpList xpickle),
                xpWrap (NotificationOperation, opOutput) xpickle]
 
instance XmlPickler WsdlParam where
    xpickle = xpWrap ( uncurry WsdlParam,
        paramName &&& paramMessage
        ) $ xpPair (xpOption (xpAttr "name" xpText))
                   (xpAttr "message" xpickle)

instance XmlPickler WsdlFault where
    xpickle = xpWrap ( uncurry WsdlFault,
        faultName &&& faultMessage
        ) $ xpPair (xpOption (xpAttr "name" xpText))
                   (xpAttr "message" xpickle)

instance XmlPickler m => XmlPickler (WsdlBinding m) where
    xpickle = xpElemWsdl "binding" $
        xpWrap (uncurry3 WsdlBinding,
            \w -> (bindingName w, bindingType w, bindingOps w)) $
        xpTriple (xpAttr "name" xpText)
                 (xpAttr "type" xpickle)
                 (xpList xpickle)

instance XmlPickler m => XmlPickler (WsdlBindingOperation m) where
    xpickle = xpElemWsdl "operation" $
        xpWrap (uncurry3 WsdlBindingOperation,
            \w -> (bindingInputs w, bindingOutputs w, bindingFaults w)) $
        xpTriple (xpList $ xpElemWsdl "input" xpickle)
                 (xpList $ xpElemWsdl "output" xpickle)
                 (xpList xpickle)


instance XmlPickler m => XmlPickler (WsdlBindingMessage m) where
    xpickle = xpWrap (uncurry WsdlBindingMessage,
        bindingMsgName &&& bindingMsg
        ) $ xpPair (xpOption (xpAttr "name" xpText))
                   xpickle 

instance XmlPickler WsdlBindingFault where
    xpickle = xpElemWsdl "fault" $
        xpWrap (WsdlBindingFault, bindingFaultName) (xpAttr "name" xpText)

instance XmlPickler WsdlService where
    xpickle = xpElemWsdl "service" $
        xpWrap (uncurry WsdlService,
            serviceName &&& servicePorts) $
        xpPair (xpAttr "name" xpText)
               (xpList xpickle)

instance XmlPickler WsdlPort where
    xpickle = xpElemWsdl "port" $
        xpWrap (uncurry WsdlPort,
            portName &&& portBinding) $
        xpPair (xpAttr "name" xpText)
               (xpAttr "binding" xpickle) 
