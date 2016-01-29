{-# LANGUAGE QuasiQuotes #-}

module ApiSpecSpec (main,spec) where

import           Control.Lens hiding ((.=))
import           Data.Aeson
import           Data.Aeson.QQ
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import           Lib.Swagger.ApiSpec
import           Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    petStoreJson <- runIO $ LazyBytes.readFile "test/pet-store-1.2.json"
    let Right v = eitherDecode' petStoreJson

    describe "meh" $ do
        let o = object [ "code" .= (400 :: Int)
                       , "message" .= ("message" :: Text)
                       , "responseModel" .= ("model" :: Text) ]
            Success resp = fromJSON o :: Result ApiSpecResponseMessage

        it "does something" $
            resp ^. apiSpecResponseMessageCode `shouldBe` 400

        it "does something" $
            resp ^. apiSpecResponseMessageMessage `shouldBe` "message"

        it "does something" $
            resp ^. apiSpecResponseMessageResponseModel `shouldBe` Just "model"

    describe "meh" $ do
        it "does something" $
            fromJSON respMessageJson `shouldBe` Success respVal

    describe "api spec" $ do

        it "parses swagger version" $
            v ^. apiSpecSwaggerVersion `shouldBe`
                "1.2"
        it "parses api version" $
            v ^. apiSpecApiVersion `shouldBe`
                Just "1.0.0"
        it "parses api base path" $
            v ^. apiSpecBasePath `shouldBe`
                "http://petstore.swagger.wordnik.com/api"
        it "parses resource path" $
            v ^. apiSpecResourcePath `shouldBe`
                Just "/pet"
        it "parses what api consumes" $
            v ^. apiSpecConsumes `shouldBe`
                Nothing
        it "parses what api produces" $
            v ^. apiSpecProduces `shouldBe`
                Just
                [ "application/json"
                , "application/xml"
                , "text/plain"
                , "text/html"]

    describe "api spec - apis" $
        it "parses apis" $
            v ^. apiSpecApis ^.. each . apiSpecApiPath `shouldBe`
                [ "/pet/{petId}"
                , "/pet"
                , "/pet/findByStatus"
                , "/pet/findByTags"
                , "/pet/uploadImage"]

    describe "api spec - /pet/{petId} endpoint" $ do
        let api = v ^. apiSpecApis
                    ^.. folded
                    . filtered (\ x -> x ^. apiSpecApiPath == "/pet/{petId}")
                    ^. singular _head
        it "has correct path" $
            api ^. apiSpecApiPath `shouldBe` "/pet/{petId}"
        it "has correct description" $
            api ^. apiSpecApiDescription `shouldBe` Nothing

        context "operations - GET" $ do
            let getOp = api ^. apiSpecOperation
                            ^.. folded
                            . filtered (\ x -> x ^. apiSpecOperationMethod == "GET")
                            ^. singular _head
            it "has correct method" $
                getOp ^. apiSpecOperationMethod `shouldBe` "GET"
            it "has correct summary" $
                getOp ^. apiSpecOperationSummary `shouldBe`
                    Just "Find pet by ID"
            it "has correct notes" $
                getOp ^. apiSpecOperationNotes `shouldBe`
                    Just "Returns a pet based on ID"
            it "has correct nickname" $
                getOp ^. apiSpecOperationNickname `shouldBe`  "getPetById"
            it "has correct authorizations" $
                getOp ^. apiSpecOperationAuthorizations `shouldBe`
                    Just Map.empty

            context "parameters" $ do
                let params = getOp ^. apiSpecOperationParameters

                context "petId" $ do
                    let petIdParam = params ^.. folded
                                            . filtered (\ x -> x ^. apiSpecParameterName == "petId")
                                            ^. singular _head
                    it "has correct param type" $
                        petIdParam ^. apiSpecParameterParamType `shouldBe` "path"
                    it "has correct name" $
                        petIdParam ^. apiSpecParameterName `shouldBe` "petId"
                    it "has correct description" $
                        petIdParam ^. apiSpecParameterDescription `shouldBe`
                            Just "ID of pet that needs to be fetched"
                    it "has correct required attribute" $
                        petIdParam ^. apiSpecParameterRequired `shouldBe`
                            Just True
                    it "has correct allowMultiple attribute" $
                        petIdParam ^. apiSpecParameterAllowMultiple `shouldBe`
                            Just False
                    it "has correct field type" $
                        petIdParam ^. apiSpecParameterDataTypeFieldType `shouldBe`
                            Just "integer"
                    it "has correct field format" $
                        petIdParam ^. apiSpecParameterDataTypeFieldFormat `shouldBe`
                            Just "int64"
                    it "has correct field default value" $
                        petIdParam ^. apiSpecParameterDataTypeFieldDefaultValue `shouldBe`
                            Nothing
                    it "has correct field enum" $
                        petIdParam ^. apiSpecParameterDataTypeFieldEnum `shouldBe`
                            Nothing
                    it "has correct field minimum" $
                        petIdParam ^. apiSpecParameterDataTypeFieldMinimum `shouldBe`
                            Just "1.0"
                    it "has correct field maximum" $
                        petIdParam ^. apiSpecParameterDataTypeFieldMaximum `shouldBe`
                            Just "100000.0"

            context "response messages" $ do
                context "400" $ do
                    let resp400 = getOp ^. apiSpecOperationResponseMessages
                                     ^. _Just
                                     ^.. folded
                                     . filtered (\ x -> x ^. apiSpecResponseMessageCode == 400)
                                     ^. singular _head
                    it "has correct code" $
                        resp400 ^. apiSpecResponseMessageCode `shouldBe` 400
                    it "has correct message" $
                        resp400 ^. apiSpecResponseMessageMessage `shouldBe`
                            "Invalid ID supplied"
                    it "has correct model" $
                        resp400 ^. apiSpecResponseMessageResponseModel `shouldBe`
                            Nothing

                context "404" $ do
                    let resp404 = getOp ^. apiSpecOperationResponseMessages
                                     ^. _Just
                                     ^.. folded
                                     . filtered (\ x -> x ^. apiSpecResponseMessageCode == 404)
                                     ^. singular _head
                    it "has correct code" $
                        resp404 ^. apiSpecResponseMessageCode `shouldBe` 404
                    it "has correct message" $
                        resp404 ^. apiSpecResponseMessageMessage `shouldBe`
                            "Pet not found"
                    it "has correct model" $
                        resp404 ^. apiSpecResponseMessageResponseModel `shouldBe`
                            Nothing

respVal :: ApiSpecResponseMessage
respVal = mempty & apiSpecResponseMessageCode .~ 400
                 & apiSpecResponseMessageMessage .~ "Invalid ID supplied"
                 & apiSpecResponseMessageResponseModel .~ Nothing

respMessageJson :: Value
respMessageJson = [aesonQQ|
          {
            "code": 400,
            "message": "Invalid ID supplied"
          }
|]
