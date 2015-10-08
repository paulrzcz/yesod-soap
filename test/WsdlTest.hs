{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module WsdlTest where

import Test.HUnitPlus
import Yesod.Soap (writeWsdl, SoapWsdl (..))

testSoapWsdl :: SoapWsdl
testSoapWsdl = undefined

writeSoapWsdl (SoapWsdl s) = writeWsdl s

wsdlGenerationTest = "wsdlGenerationTest" ~: ( do
    str <- writeSoapWsdl testSoapWsdl
    putStrLn str
    )