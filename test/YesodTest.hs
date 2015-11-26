{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Main where

import Yesod
import Yesod.Soap
import Yesod.Soap.Subsite

import Text.XML.HXT.Core

data Master = Master {
    getSoap :: SoapServer
}

mkYesod "Master" [parseRoutes|
/ HomeR GET
/soap SubsiteR SoapServer getSoap
|]

instance Yesod Master

getHomeR :: HandlerT Master IO Html
getHomeR = undefined

defaultMsgs :: [WsdlMessage]
defaultMsgs = [WsdlMessage {
    msgName = "helloRequest",
    msgParts = [WsdlMessagePart {
        partName = "name",
        partElement = Nothing,
        partType = Just (mkQName "xsd" "string" "")
    }]
}, WsdlMessage {
    msgName = "helloResponse",
    msgParts = [WsdlMessagePart {
        partName = "greeting",
        partElement = Nothing,
        partType = Just (mkQName "xsd" "string" "")
    }]
}]

defaultPort = WsdlPortType {
    portTypeName = "helloSoap_Port",
    portTypeOperations = [
        WsdlPortOperation {
            operationName = "sayHello",
            operationParameterOrder = Nothing,
            operationDesc = [
                RequestResponse {
                    opInput = WsdlParam Nothing (mkName "helloRequest"),
                    opOutput = WsdlParam Nothing (mkName "helloResponse"),
                    opFault = []
            }]
    }]
}

defaultBinding :: WsdlBinding SoapBinding
defaultBinding = undefined

testSoapWsdl :: SoapWsdl
testSoapWsdl = SoapWsdl
    Wsdl {
        wsdlNameSpace = Just "http://hsoap.velrina.com",
        wsdlName = Just "emptySoap",
        wsdlMessages = defaultMsgs,
        wsdlPortTypes = [defaultPort],
        wsdlBindings = [],
        wsdlServices = []
    }


main = warp 3000 $ Master (SoapServer testSoapWsdl)