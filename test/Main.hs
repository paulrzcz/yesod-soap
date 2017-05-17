module Main where

import           Test.HUnitPlus
import           Text.XML.HXT.Core
import           WsdlTest
import           Yesod.Soap

testSoapEnvelope = "<s:Envelope xmlns:s=\"http://www.w3.org/2001/12/soap-envelope\">\n\
\  <s:Body>\n\
\    <id>Dummy body</id>\n\
\  </s:Body>\n\
\</s:Envelope>\n"

testSoapEnvelopeWithEncoding = "<s:Envelope xmlns:s=\"http://www.w3.org/2001/12/soap-envelope\" s:encodingStyle=\"http://www.w3.org/2001/12/soap-encoding\">\n\
\  <s:Body>\n\
\    <id>Dummy body</id>\n\
\  </s:Body>\n\
\</s:Envelope>\n"

testSoapEnvelopeWithRtcf = "<s:Envelope xmlns:s=\"http://www.w3.org/2001/12/soap-envelope\" xmlns:rtcf=\"http://rtcf.velrina.com\">\n\
\  <s:Body>\n\
\    <rtcf:id>Dummy body</rtcf:id>\n\
\  </s:Body>\n\
\</s:Envelope>\n"

testSoapEnvelopeWithHeader = "<s:Envelope xmlns:s=\"http://www.w3.org/2001/12/soap-envelope\">\n\
\  <s:Header>\n\
\    <wscoor:CoordinationContext xmlns:wsa=\"http://schemas.xmlsoap.org/ws/2004/08/addressing\" xmlns:wscoor=\"http://docs.oasis-open.org/ws-tx/wscoor/2006/06\">\n\
\      <wscoor:Identifier>identifier</wscoor:Identifier>\n\
\      <wscoor:Expires>3200</wscoor:Expires>\n\
\      <wscoor:CoordinationType>coordtype</wscoor:CoordinationType>\n\
\      <wscoor:RegistrationService>\n\
\        <wsa:Address>address</wsa:Address>\n\
\      </wscoor:RegistrationService>\n\
\    </wscoor:CoordinationContext>\n\
\  </s:Header>\n\
\  <s:Body>\n\
\    <id>Dummy body</id>\n\
\  </s:Body>\n\
\</s:Envelope>\n"

type StringEnvelope = SoapEnvelope IdBody

type RtcfEnvelope = SoapEnvelope RtcfBody

data RtcfBody = RtcfBody String
    deriving (Show)

data IdBody = IdBody String
    deriving (Show)

xpElemRtcf = xpElemNS "http://rtcf.velrina.com" "rtcf"

instance XmlPickler RtcfBody where
    xpickle = xpElemRtcf "id" $
        xpWrap (\s -> RtcfBody s,
            \(RtcfBody s) -> s
            ) $
        xpText

instance XmlPickler IdBody where
    xpickle = xpElem "id" $
        xpWrap (\s -> IdBody s,
            \(IdBody s) -> s
            ) $
        xpText

serializeTest = "serializeTest" ~: ( do
    let se = SoapEnvelope Nothing (SoapBody (IdBody "Dummy body"))
    str <- writeEnvelope se
    putStrLn str
    assertEqual "xml text" testSoapEnvelope str)

header = SoapHeader (Just (WsCoordinationContext "identifier" 3200 "coordtype" (WsRegistrationService "address")))

serializeTestWithHeader = "serializeTest" ~: ( do
    let se = SoapEnvelope (Just header) (SoapBody (IdBody "Dummy body"))
    str <- writeEnvelope se
    putStrLn str
    assertEqual "xml text" testSoapEnvelopeWithHeader str)

deserializeTest = "deserializeTest" ~: ( do
    se <- (readEnvelope testSoapEnvelope) :: IO (Either String StringEnvelope)
    case se of
        Left x  -> assertString x
        Right x -> putStrLn $ show x
    )

deserializeTest2 = "deserializeTest with encodingStyle" ~: ( do
    se <- (readEnvelope testSoapEnvelopeWithEncoding) :: IO (Either String StringEnvelope)
    case se of
        Left x  -> assertString x
        Right x -> putStrLn $ show x
    )

deserializeTest3 = "deserializeTest with custom namespace" ~: ( do
    se <- (readEnvelope testSoapEnvelopeWithRtcf) :: IO (Either String RtcfEnvelope)
    case se of
        Left x  -> assertString x
        Right x -> putStrLn $ show x
    )


suite = TestSuite {
    suiteName = "Basic serialization",
    suiteTests = [deserializeTest, serializeTest, deserializeTest2, deserializeTest3, serializeTestWithHeader,
        wsdlGenerationTest],
    suiteConcurrently = False,
    suiteOptions = []
}

main = createMain [suite]
