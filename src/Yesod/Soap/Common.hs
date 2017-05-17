module Yesod.Soap.Common
    ( xpElemSoap
    , xpElemAddr
    , xpElemCoor
    , xpElemWsdl
    , xpElemWsdlSoap
    , soapPrefix
    , coorPrefix
    , addrPrefix
    , wsdlPrefix
    , wsdlSoapPrefix
    , nsSoap
    , nsAddr
    , nsCoor
    , nsWsdl
    , nsWsdlSoap
    ) where

import           Text.XML.HXT.Core (xpElemNS)

soapPrefix = "s"
addrPrefix = "wsa"
coorPrefix = "wscoor"
wsdlPrefix = "wsdl"
wsdlSoapPrefix = "ws"

nsSoap = "http://www.w3.org/2001/12/soap-envelope"
nsAddr = "http://schemas.xmlsoap.org/ws/2004/08/addressing"
nsCoor = "http://docs.oasis-open.org/ws-tx/wscoor/2006/06"
nsWsdl = "http://schemas.xmlsoap.org/wsdl/"
nsWsdlSoap = "http://schemas.xmlsoap.org/wsdl/soap/"

xpElemSoap = xpElemNS nsSoap soapPrefix
xpElemAddr = xpElemNS nsAddr addrPrefix
xpElemCoor = xpElemNS nsCoor coorPrefix
xpElemWsdl = xpElemNS nsWsdl wsdlPrefix
xpElemWsdlSoap = xpElemNS nsWsdlSoap wsdlSoapPrefix
