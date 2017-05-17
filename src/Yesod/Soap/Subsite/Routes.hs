{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-} -- QuasiQuoter
module Yesod.Soap.Subsite.Routes
  ( parseSoap
  , parseSoapFile
  , parseSoapFileWith
  ) where

import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import qualified System.IO                  as SIO

data ResourceHandlers = RH {

} deriving (Show)

instance Lift ResourceHandlers where
  lift RH = [|RH|]

parseSoap :: QuasiQuoter
parseSoap = QuasiQuoter { quoteExp = x }
  where
    x inputString = lift (resourcesFromString inputString)

parseSoapFile :: FilePath -> Q Exp
parseSoapFile = parseSoapFileWith parseSoap

parseSoapFileWith :: QuasiQuoter -> FilePath -> Q Exp
parseSoapFileWith qq fp = do
    qAddDependentFile fp
    s <- qRunIO $ readUtf8File fp
    quoteExp qq s

readUtf8File :: FilePath -> IO String
readUtf8File fp = do
    h <- SIO.openFile fp SIO.ReadMode
    SIO.hSetEncoding h SIO.utf8_bom
    SIO.hGetContents h

resourcesFromString :: String -> [ResourceHandlers]
resourcesFromString = undefined
