{-# LANGUAGE DeriveGeneric #-}

module Lib.Swagger where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)


data ResourceListing = ResourceListing
    { rlswaggerVersion :: Text
    , rlApis :: [ResourceObject]
    , rlApiVersion :: Maybe Text
    , rlInfo :: Maybe Info
    , rlAuthorizations :: Maybe [Authorization]
    } deriving (Eq,Show,Generic)

data ResourceObject = ResourceObject
    { roPath :: Text
    , roDescription :: Maybe Text
    } deriving (Eq,Show,Generic)

data Info = Info
    { iTitle :: Text
    , iDescription :: Text
    , iTermsOfServiceUrl :: Maybe Text
    , iContact :: Maybe Text
    , iLicense :: Maybe Text
    , iLicenseurl :: Maybe Text
    } deriving (Eq,Show,Generic)

data Authorization =
    Authorization
    deriving (Eq,Show,Generic)
