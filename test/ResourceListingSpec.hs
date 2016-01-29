 module ResourceListingSpec (main, spec) where

import Lib.Swagger.ResourceListing
import Test.Hspec
import qualified Data.ByteString.Lazy as LazyBytes
import Data.Aeson
import qualified Data.HashMap.Strict as Map

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    petStoreJson <- runIO $  LazyBytes.readFile "test/resourceListing.json"
    let Right v = eitherDecode' petStoreJson

    describe "resource listing" $
        it "parses pet store resource listing" $
            v `shouldBe` petStore

petStore :: ResourceListing
petStore =
    ResourceListing
    { resourceListingSwaggerVersion = "1.2"
    , resourceListingApis = [ ResourceObject
                              { resourceObjectPath = "/pet"
                              , resourceObjectDescription = Just
                                    "Operations about pets"
                              }
                            , ResourceObject
                              { resourceObjectPath = "/user"
                              , resourceObjectDescription = Just
                                    "Operations about user"
                              }
                            , ResourceObject
                              { resourceObjectPath = "/store"
                              , resourceObjectDescription = Just
                                    "Operations about store"
                              }]
    , resourceListingApiVersion = Just "1.0.0"
    , resourceListingInfo = Just
          (Info
           { infoTitle = "Swagger Sample App"
           , infoDescription = "This is a sample server Petstore server.  You can find out more about Swagger \n    at <a href=\"http://swagger.wordnik.com\">http://swagger.wordnik.com</a> or on irc.freenode.net, #swagger.  For this sample,\n    you can use the api key \"special-key\" to test the authorization filters"
           , infoTermsOfServiceUrl = Just "http://swagger.io/terms/"
           , infoContact = Just "apiteam@wordnik.com"
           , infoLicense = Just "Apache 2.0"
           , infoLicenseurl = Just
                 "http://www.apache.org/licenses/LICENSE-2.0.html"
           })
    , resourceListingAuthorizations = Just
          (Map.fromList
               [ ( "oauth2"
                 , Authorization
                   { authorizationType = "oauth2"
                   , authorizationPassAs = Nothing
                   , authorizationScopes = [ AuthScope
                                             { authScopeScope = "email"
                                             , authScopeDescription = Just
                                                   "Access to your email address"
                                             }
                                           , AuthScope
                                             { authScopeScope = "pets"
                                             , authScopeDescription = Just
                                                   "Access to your pets"
                                             }]
                   , authorizationGrantType = Map.fromList
                         [ ( "implicit"
                           , ImplicitGrant
                             { implicitGrantLoginEndPoint = LoginEndpoint
                               { loginEndPointUrl = "http://petstore.swagger.wordnik.com/oauth/dialog"
                               }
                             , implicitGrantTokenName = Just "access_token"
                             })
                         , ( "authorization_code"
                           , AuthorizationCodeGrant
                             { authorizationCodeGrantTokenRequestEndpoint = TokenRequestEndpoint
                               { tokenRequestEndpointUrl = "http://petstore.swagger.wordnik.com/oauth/requestToken"
                               , tokenRequestEndpointClientIdName = Just
                                     "client_id"
                               , tokenRequestEndpointClientSecretName = Just
                                     "client_secret"
                               }
                             , authorizationCodeGranttokenEndPoint = TokenEndpoint
                               { tokenEndPointUrl = "http://petstore.swagger.wordnik.com/oauth/token"
                               , tokenEndPointTokenName = Just "access_code"
                               }
                             })]
                   })])
    }
