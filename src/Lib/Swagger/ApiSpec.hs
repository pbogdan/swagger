{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Swagger.ApiSpec where

import           Control.Lens hiding ((.=))
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.ByteString.Lazy as LazyBytes
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           GHC.Generics

-- import qualified Data.HashMap.Strict as Map
-- import Data.Aeson.Types (Parser)
-- import Data.Foldable (asum)

-- loadTest :: IO LazyBytes.ByteString
-- loadTest = LazyBytes.readFile "test/pet-store-1.2.json"

-- Right v <- eitherDecode <$> LazyBytes.readFile "test/pet-store-1.2.json" :: IO (Either String ApiSpec)
-- v ^. apiSpecApis . traversed . apiSpecOperation ^. singular _head

data ApiSpec = ApiSpec
    { _apiSpecSwaggerVersion :: Text
    , _apiSpecApiVersion :: Maybe Text
    , _apiSpecBasePath :: Text
    , _apiSpecResourcePath :: Maybe Text
    , _apiSpecApis :: [ApiSpecApi]
    , _apiSpecModels :: Maybe (HashMap Text ApiSpecModel)
    , _apiSpecProduces :: Maybe [Text]
    , _apiSpecConsumes :: Maybe [Text]
    , _apiSpecAuthorizations :: Maybe (HashMap Text [ApiSpecScope])
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpec where
    parseJSON (Object o ) =
        ApiSpec <$> o .: "swaggerVersion"
            <*> o .:? "apiVersion"
            <*> o .: "basePath"
            <*> o .:? "resourcePath"
            <*> o .: "apis"
            <*> o .: "models"
            <*> o .:? "produces"
            <*> o .:? "consumes"
            <*> o .:? "authorizations"
    parseJSON _ = fail "LOL"

data ApiSpecApi = ApiSpecApi
    { _apiSpecApiPath :: Text
    , _apiSpecApiDescription :: Maybe Text
    , _apiSpecOperation :: [ApiSpecOperation]
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecApi where
    parseJSON (Object o) =
        ApiSpecApi <$> o .: "path"
            <*> o .:? "description"
            <*> o .: "operations"

-- @TODO: missing type
data ApiSpecOperation = ApiSpecOperation
    { _apiSpecOperationMethod :: Text
    , _apiSpecOperationSummary :: Maybe Text
    , _apiSpecOperationNotes :: Maybe Text
    , _apiSpecOperationNickname :: Text
    , _apiSpecOperationAuthorizations :: Maybe (HashMap Text [ApiSpecScope])
    , _apiSpecOperationParameters :: [ApiSpecParameter]
    , _apiSpecOperationResponseMessages :: Maybe [ApiSpecResponseMessage]-- @TODO: check against spec
    , _apiSpecOperationProduces :: Maybe [Text] -- @TODO: check against spec
    , _apiSpecOperationConsumes :: Maybe [Text]-- @TODO: check against spec
    , _apiSpecOperationDeprecated :: Maybe Text
    } deriving (Eq,Show,Generic)


instance FromJSON ApiSpecOperation where
    parseJSON (Object o) =
        ApiSpecOperation <$> o .: "method"
            <*> o .:? "summary"
            <*> o .:? "notes"
            <*> o .: "nickname"
            <*> o .:? "authorizations"
            <*> o .: "parameters"
            <*> o .:? "responseMessages"
            <*> o .:? "produces"
            <*> o .:? "consumes"
            <*> o .:? "deprecated"

-- @TODO: this doesn't look right as the pet store test case has parameters with
-- their type, format, etc. not present in this type definition

-- @TODO: see the type description params can be factored out and shared with
-- the ones further down
data ApiSpecParameter = ApiSpecParameter
    { _apiSpecParameterParamType :: Text
    , _apiSpecParameterName :: Text
    , _apiSpecParameterDescription :: Maybe Text
    , _apiSpecParameterRequired :: Maybe Bool
    , _apiSpecParameterAllowMultiple :: Maybe Bool

    , _apiSpecParameterDataTypeFieldType :: Maybe Text
    , _apiSpecParameterDataTypeFieldFormat :: Maybe Text
    , _apiSpecParameterDataTypeFieldDefaultValue :: Maybe Text
    , _apiSpecParameterDataTypeFieldEnum :: Maybe [Text]
    , _apiSpecParameterDataTypeFieldMinimum :: Maybe Text
    , _apiSpecParameterDataTypeFieldMaximum :: Maybe Text
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecParameter where
    parseJSON (Object o) =
        ApiSpecParameter <$> o .: "paramType"
            <*> o .: "name"
            <*> o .:? "description"
            <*> o .:? "required"
            <*> o .:? "allowMultiple"
            <*> o .:? "type"
            <*> o .:? "format"
            <*> o .:? "defaultValue"
            <*> o .:? "enum"
            <*> o .:? "minimum"
            <*> o .:? "maximum"

newtype HttpStatusCode = HttpStatusCode
    { getHttpStatusCode :: Integer
    }

deriving instance Eq HttpStatusCode
deriving instance Show HttpStatusCode
deriving instance Generic HttpStatusCode
deriving instance FromJSON HttpStatusCode

instance Monoid HttpStatusCode where
    mempty = HttpStatusCode 0
    mappend x1 x2
        | x2 == mempty = x1
        | otherwise = x2

data ApiSpecResponseMessage = ApiSpecResponseMessage
    { _apiSpecResponseMessageCode :: HttpStatusCode
    , _apiSpecResponseMessageMessage :: Text
    , _apiSpecResponseMessageResponseModel :: Maybe Text
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecResponseMessage where
    parseJSON (Object o) =
        ApiSpecResponseMessage <$> o .: "code"
            <*> o .: "message"
            <*> o .:? "responseModel"

data ApiSpecModel = ApiSpecModel
    { apiSpecModelId :: Text
    , apiSpecModelDescription :: Maybe Text
    , apiSpecModelRequired :: Maybe [Text]
    , apiSpecModelProperties :: Maybe (HashMap Text ApiDataTypeField)
    , apiSpecModelSubTypes :: Maybe [Text]
    , apiSpecModelDiscriminator :: Maybe Text
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecModel where
    parseJSON (Object o) =
        ApiSpecModel <$> o .: "id"
            <*> o .:? "description"
            <*> o .:? "required"
            <*> (parseJSON =<< o .: "properties")
            <*> o .:? "subTypes"
            <*> o .:? "discriminator"

-- data ApiSpecProperty = ApiSpecProperty
--     {
--     }

-- @TODO: this would be better off split into either multiple definitions or an
-- ADT with multiple constructors however not sure how we would avoid field name
-- duplication then :-(
data ApiDataTypeField = ApiDataTypeField
    { apiDataTypeFieldType :: Maybe Text
    , apiDataTypeFieldRef :: Maybe Text
    , apiDataTypeFieldFormat :: Maybe Text
    , apiDataTypeFieldDefaultValue :: Maybe Text
    , apiDataTypeFieldEnum :: Maybe [Text]
    , apiDataTypeFieldMinimum :: Maybe Text
    , apiDataTypeFieldMaximum :: Maybe Text
    , apiDataTypeFieldItems :: Maybe ApiSpecItems
    , apiDataTypeFieldUniqueItems :: Maybe Bool
    } deriving (Eq,Show,Generic)

instance FromJSON ApiDataTypeField where
    parseJSON (Object o) =
        ApiDataTypeField <$> o .:? "type"
            <*> o .:? "$ref"
            <*> o .:? "format"
            <*> o .:? "defaultValue"
            <*> o .:? "enum"
            <*> o .:? "minimum"
            <*> o .:? "maximum"
            <*> o .:? "items"
            <*> o .:? "uniqueItems"

data ApiSpecItems = ApiSpecItems
    { apiSpecItemsType :: Maybe Text
    , apiSpecItemsRef :: Maybe Text
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecItems where
    parseJSON (Object o) =
        ApiSpecItems <$> o .:? "type" <*> o .:? "ref"

-- data ApiDataType
--     = Primitive
--     | Container
--     | Complex
--     | Void
--     | File


-- data ApiSpecAuthorization = ApiSpecAuthorization
--     { apiSpecAuthorizationScopes :: [ApiSpecScope]
--     } deriving (Eq,Show,Generic)

data ApiSpecScope = ApiSpecScope
    { apiSpecScope :: Text
    , apiSpecDescription :: Maybe Text
    } deriving (Eq,Show,Generic)

instance FromJSON ApiSpecScope where
    parseJSON (Object o) =
        ApiSpecScope <$> o .: "scope" <*> o .:? "description"

makeLenses ''ApiSpec
makeLenses ''ApiSpecApi
makeLenses ''ApiSpecParameter
makeLenses ''ApiSpecOperation
makeLenses ''ApiSpecResponseMessage

instance Monoid ApiSpecResponseMessage where
    mempty = ApiSpecResponseMessage mempty mempty mempty
    mappend a1 a2
        | a2 == mempty = a1
        | otherwise = a2
