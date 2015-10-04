module Main where

import Test.HUnitPlus
import Yesod.Soap
import Text.XML.HXT.Core

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


type StringEnvelope = SoapEnvelope IdBody

data IdBody = IdBody String
    deriving (Show)

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

deserializeTest = "deserializeTest" ~: ( do
    se <- (readEnvelope testSoapEnvelope) :: IO (Either String StringEnvelope)
    case se of
        Left x -> assertString x
        Right x -> putStrLn $ show x
    )

deserializeTest2 = "deserializeTest with encodingStyle" ~: ( do
    se <- (readEnvelope testSoapEnvelopeWithEncoding) :: IO (Either String StringEnvelope)
    case se of
        Left x -> assertString x
        Right x -> putStrLn $ show x
    )


suite = TestSuite {
    suiteName = "Basic serialization",
    suiteTests = [deserializeTest, serializeTest, deserializeTest2],
    suiteConcurrently = False,
    suiteOptions = []
}

main = createMain [suite]