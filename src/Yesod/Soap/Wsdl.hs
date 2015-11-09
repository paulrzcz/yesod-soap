{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, UndecidableInstances #-}
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
    , WsdlBindingMessageInput (..)
    , WsdlBindingMessageOutput (..)
    , WsdlBindingFault (..)
    , WsdlService (..)
    , WsdlPort (..)
    , QName (..)
    , readWsdl
    , writeWsdl
    , SoapWsdl (..)
    ) where

import Data.Proxy
import Control.Arrow((&&&))
import Text.XML.HXT.Core

import Yesod.Soap.Bindings
import Yesod.Soap.Common
import Yesod.Soap.Types(writeConfig, readConfig)

newtype SoapWsdl = SoapWsdl (Wsdl SoapBinding)

data Binding m => Wsdl m = Wsdl { -- definitions
    wsdlNameSpace :: Maybe String,
    wsdlName :: Maybe String,
    wsdlMessages :: [WsdlMessage],
    wsdlPortTypes :: [WsdlPortType],
    wsdlBindings  :: [WsdlBinding m],
    wsdlServices  :: [WsdlService]
}

data WsdlMessage = WsdlMessage {
    msgName :: String,
    msgParts :: [WsdlMessagePart]
}

data WsdlMessagePart = WsdlMessagePart {
    partName :: String,
    partElement :: Maybe QName,
    partType :: Maybe QName
}

data WsdlPortType = WsdlPortType {
    portTypeName :: String,
    portTypeOperations :: [WsdlPortOperation]
}

data WsdlPortOperation = WsdlPortOperation {
    operationName :: String,
    operationParameterOrder :: Maybe String,
    operationDesc :: [WsdlOperation]
}

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

data Binding m => WsdlBinding m = WsdlBinding {
    bindingName :: String,
    bindingType :: QName,
    bindingCustom :: Maybe (GBinding m),
    bindingOps  :: [WsdlBindingOperation m]
}

data Binding m => WsdlBindingOperation m = WsdlBindingOperation {
    bindingOpCustom :: Maybe (GOperation m),
    bindingInputs :: [WsdlBindingMessageInput m],
    bindingOutputs :: [WsdlBindingMessageOutput m],
    bindingFaults :: [WsdlBindingFault m]
}

data Binding m => WsdlBindingMessageInput m = WsdlBindingMessageInput {
    bindingMsgInputName :: Maybe String,
    bindingMsgInput :: GInput m
}

data Binding m =>  WsdlBindingMessageOutput m = WsdlBindingMessageOutput {
    bindingMsgOutputName :: Maybe String,
    bindingMsgOutput :: GOutput m
}

data Binding m => WsdlBindingFault m = WsdlBindingFault {
    bindingFaultName :: String,
    bindingFaultMsg :: Maybe (GFault m)
}

data WsdlService = WsdlService {
    serviceName :: String,
    servicePorts :: [WsdlPort]
} deriving (Show)

data WsdlPort = WsdlPort {
    portName :: String,
    portBinding :: QName
} deriving (Show)

-- read / write section

readWsdl :: (Binding m, XmlPickler (GBinding m), XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => String -> IO (Either String (Wsdl m))
readWsdl str = do 
    xs <- runX $ readString readConfig str 
    case xs of
        [] -> return (Left "No WSDL found")
        [x] -> return $ unpickleDoc' xpWsdl x
        _ -> return (Left "Too many xml trees in input")

writeWsdl :: (Binding m, XmlPickler (GBinding m), XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => Wsdl m -> IO String
writeWsdl envelope = do
    let xmlTree = pickleDoc xpWsdl envelope
    [str] <- runX (
        constA xmlTree
        >>>
        writeDocumentToString writeConfig
        )
    return str

-- XmlPicker

xpWsdl :: (Binding m, XmlPickler (GBinding m), XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => PU (Wsdl m)
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

instance (Binding m, XmlPickler (GBinding m), XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => XmlPickler (Wsdl m) where
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
                 (xpAttrImplied "element" xpickle)
                 (xpAttrImplied "type" xpickle)

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

instance (Binding m, XmlPickler (GBinding m), XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => XmlPickler (WsdlBinding m) where
    xpickle = xpElemWsdl "binding" $
        xpWrap (uncurry4 WsdlBinding,
            \w -> (bindingName w, bindingType w, bindingCustom w, bindingOps w)) $
        xp4Tuple (xpAttr "name" xpText)
                 (xpAttr "type" xpickle)
                 (xpOption xpickle)
                 (xpList xpickle)


instance (Binding m, XmlPickler (GOperation m), XmlPickler (GInput m), XmlPickler (GOutput m), XmlPickler (GFault m)) 
        => XmlPickler (WsdlBindingOperation m) where
    xpickle = xpElemWsdl "operation" $
        xpWrap (uncurry4 WsdlBindingOperation,
            \w -> (bindingOpCustom w, bindingInputs w, bindingOutputs w, bindingFaults w)) $
        xp4Tuple (xpOption (xpickleOperation (Proxy :: Proxy m)))
                 (xpList xpickle)
                 (xpList xpickle)
                 (xpList xpickle)


instance (Binding m, XmlPickler (GInput m)) => XmlPickler (WsdlBindingMessageInput m) where
    xpickle = xpElemWsdl "input" $
        xpWrap (uncurry WsdlBindingMessageInput,
        bindingMsgInputName &&& bindingMsgInput
        ) $ xpPair (xpOption (xpAttr "name" xpText))
                   (xpickleInput (Proxy :: Proxy m))

instance (Binding m, XmlPickler (GOutput m)) => XmlPickler (WsdlBindingMessageOutput m) where
    xpickle = xpElemWsdl "output" $
        xpWrap (uncurry WsdlBindingMessageOutput,
        bindingMsgOutputName &&& bindingMsgOutput
        ) $ xpPair (xpOption (xpAttr "name" xpText))
                   (xpickleOutput (Proxy :: Proxy m))

instance (Binding m, XmlPickler (GFault m)) => XmlPickler (WsdlBindingFault m) where
    xpickle = xpElemWsdl "fault" $
        xpWrap (uncurry WsdlBindingFault, 
            bindingFaultName &&& bindingFaultMsg) $
        xpPair (xpAttr "name" xpText) (xpOption (xpickleFault (Proxy :: Proxy m)))

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
