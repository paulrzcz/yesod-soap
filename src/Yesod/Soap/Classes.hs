module Yesod.Soap.Classes (
    ) where

import Yesod.Soap.Wsdl (WsdlMessage)

class ToWsdlMessage m where
    toWsdlMessage :: m -> WsdlMessage