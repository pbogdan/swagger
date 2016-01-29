{-# LANGUAGE DeriveGeneric #-}

module Lib.Swagger.ResourceListing where

import qualified Data.ByteString.Lazy as LazyBytes
import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Aeson.Types (Parser)
import Data.Foldable (asum)

loadTest :: IO LazyBytes.ByteString
loadTest = LazyBytes.readFile "test/resourceListings.json"

data ResourceListing = ResourceListing
    { resourceListingSwaggerVersion :: Text
    , resourceListingApis :: [ResourceObject]
    , resourceListingApiVersion :: Maybe Text
    , resourceListingInfo :: Maybe Info
    , resourceListingAuthorizations :: Maybe (HashMap Text Authorization)
    } deriving (Eq,Show,Generic)

data ResourceObject = ResourceObject
    { resourceObjectPath :: Text
    , resourceObjectDescription :: Maybe Text
    } deriving (Eq,Show,Generic)

data Info = Info
    { infoTitle :: Text
    , infoDescription :: Text
    , infoTermsOfServiceUrl :: Maybe Text
    , infoContact :: Maybe Text
    , infoLicense :: Maybe Text
    , infoLicenseurl :: Maybe Text
    } deriving (Eq,Show,Generic)

data Authorization = Authorization
    { authorizationType :: Text
    , authorizationPassAs :: Maybe Text
    , authorizationScopes :: [AuthScope]
    , authorizationGrantType :: HashMap Text AuthGrantType
    } deriving (Eq,Show,Generic)

data AuthScope = AuthScope
    { authScopeScope :: Text
    , authScopeDescription :: Maybe Text
    } deriving (Eq,Show,Generic)

data AuthGrantType
    = ImplicitGrant { implicitGrantLoginEndPoint :: LoginEndpoint
                    , implicitGrantTokenName :: Maybe Text}
    | AuthorizationCodeGrant { authorizationCodeGrantTokenRequestEndpoint :: TokenRequestEndpoint
                             , authorizationCodeGranttokenEndPoint :: TokenEndpoint}
    deriving (Eq,Show,Generic)

data LoginEndpoint = LoginEndpoint
    { loginEndPointUrl :: Text
    } deriving (Eq,Show,Generic)

data TokenRequestEndpoint = TokenRequestEndpoint
    { tokenRequestEndpointUrl :: Text
    , tokenRequestEndpointClientIdName :: Maybe Text
    , tokenRequestEndpointClientSecretName :: Maybe Text
    } deriving (Eq,Show,Generic)

data TokenEndpoint = TokenEndpoint
    { tokenEndPointUrl :: Text
    , tokenEndPointTokenName :: Maybe Text
    } deriving (Eq,Show,Generic)

-- ****************************************************************************

instance FromJSON ResourceListing where
    parseJSON (Object o) =
        ResourceListing <$> o .: "swaggerVersion"
            <*> (parseJSON =<< o .: "apis")
            <*> o .:? "apiVersion"
            <*> o .:? "info"
            <*> parseAuthorizations o
    parseJSON _ = fail "Unable to parse ResourceListing object"

parseAuthorizations :: Object -> Parser (Maybe (HashMap Text Authorization))
parseAuthorizations o = do
    let auths = Map.lookup "authorizations" o
    case auths of
        Nothing -> return Nothing
        Just x -> parseJSON x

instance FromJSON ResourceObject where
    parseJSON (Object o) =
        ResourceObject <$> o .: "path"
            <*> o .: "description"
    parseJSON _ = fail "Unable to parse ResourceObject object"

instance FromJSON Info where
    parseJSON (Object o) =
        Info <$> o .: "title"
            <*> o .: "description"
            <*> o .:? "termsOfServiceUrl"
            <*> o .:? "contact"
            <*> o .:? "license"
            <*> o .:? "licenseUrl"
    parseJSON _ = fail "Unable to parse Info object"

instance FromJSON Authorization where
    parseJSON (Object o ) =
        Authorization <$> o .: "type"
            <*> o .:? "passAs"
            <*> (parseJSON =<< o .: "scopes")
            <*> parseGrantTypes o
    parseJSON _ = fail "Unable to parse Authorization object"

parseGrantTypes :: Object -> Parser (HashMap Text AuthGrantType)
parseGrantTypes o = do
    let grants = Map.lookup "grantTypes" o
    case grants of
        Nothing -> fail "grantTypes not found"
        Just x -> parseJSON x

instance FromJSON AuthScope where
    parseJSON (Object o) =
        AuthScope <$> o .: "scope" <*> o .:? "description"
    parseJSON _ = fail "Unable to parse AuthScope"

instance FromJSON AuthGrantType where
    parseJSON (Object o) =
        asum
            [ ImplicitGrant <$> (parseJSON =<< o .: "loginEndpoint") <*>
              o .:? "tokenName"
            , AuthorizationCodeGrant <$>
              (parseJSON =<< o .: "tokenRequestEndpoint") <*>
              (parseJSON =<< o .: "tokenEndpoint")
            , return $ ImplicitGrant (LoginEndpoint "trol.com") Nothing
            ]
    parseJSON _ = fail "Unable to parse AuthGrantType"

instance FromJSON LoginEndpoint where
    parseJSON (Object o) = LoginEndpoint <$> o .: "url"
    parseJSON _ = fail "Unable to parse LoginEndpoint"

instance FromJSON TokenRequestEndpoint where
    parseJSON (Object o) =
        TokenRequestEndpoint <$> o .: "url"
            <*> o .:? "clientIdName"
            <*> o .:? "clientSecretName"
    parseJSON _ = fail "Unable to parse TokenRequestEndpoint"


instance FromJSON TokenEndpoint where
    parseJSON (Object o) =
        TokenEndpoint <$> o .: "url"
            <*> o .:? "tokenName"
    parseJSON _ = fail "Unable to parse TokenEndpoint"
