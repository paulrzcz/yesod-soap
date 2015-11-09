module WsdlTest where

import Test.HUnitPlus
import Yesod.Soap
import Text.XML.HXT.Core

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

writeSoapWsdl (SoapWsdl s) = writeWsdl s

wsdlGenerationTest = "wsdlGenerationTest" ~: ( do
    str <- writeSoapWsdl testSoapWsdl
    putStrLn str
    )