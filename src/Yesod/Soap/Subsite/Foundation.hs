{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Yesod.Soap.Subsite.Foundation where

import Data.Text (Text)

import Yesod.Core

import Yesod.Soap.Wsdl

data SoapServer = SoapServer SoapWsdl

mkYesodSubData "SoapServer" [parseRoutes|
/ SoapR GET POST
/wsdl SoapWsdlR GET
/xsd SoapXsdR GET
|]

type SoapHandler a = forall master. Yesod master => HandlerT SoapServer (HandlerT master IO) a
