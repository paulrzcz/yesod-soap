{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
module Yesod.Soap.Subsite.Dispatch
    ( getSoapWsdlR
    ) where

import           Yesod.Core
import           Yesod.Soap
import           Yesod.Soap.Subsite.Foundation

getSoapWsdlR :: SoapHandler RepXml
getSoapWsdlR = do
    (SoapServer (SoapWsdl wsdl)) <- getYesod
    wsdlString <- liftIO $ writeWsdl wsdl
    return $ repXml wsdlString

getSoapXsdR :: SoapHandler RepXml
getSoapXsdR = undefined

getSoapR :: SoapHandler RepXml
getSoapR = do
    isWsdl <- lookupGetParam "wsdl"
    isXsd <- lookupGetParam "xsd"
    properFunc isWsdl isXsd

properFunc (Just _) Nothing = getSoapWsdlR
properFunc Nothing (Just _) = getSoapXsdR
properFunc _ _              = notFound

postSoapR :: SoapHandler RepXml
postSoapR = undefined

instance Yesod master => YesodSubDispatch SoapServer (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesSoapServer)
