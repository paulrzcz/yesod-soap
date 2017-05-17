{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Yesod.Soap.Subsite.Foundation where

import           Data.Text       (Text)

import           Yesod.Core

import           Yesod.Soap.Wsdl

data SoapServer = SoapServer SoapWsdl

mkYesodSubData "SoapServer" [parseRoutes|
/ SoapR GET POST
|]

type SoapHandler a = forall master. Yesod master => HandlerT SoapServer (HandlerT master IO) a
