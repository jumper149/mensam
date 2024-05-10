module Mensam.API.Route.Api.User where

import Mensam.API.Aeson
import Mensam.API.Aeson.StaticText
import Mensam.API.Data.User
import Mensam.API.Data.User.Password
import Mensam.API.Data.User.Username

import Data.Aeson qualified as A
import Data.Kind
import Data.Text qualified as T
import Data.Time qualified as T
import Deriving.Aeson qualified as A
import GHC.Generics
import Servant.API hiding (BasicAuth)
import Servant.Auth
import Servant.Auth.JWT.WithSession
import Text.Email.Parser

type Routes :: Type -> Type
data Routes route = Routes
  { routeLogin ::
      route
        :- Summary "Login"
          :> Description
              "Login to your user account.\n"
          :> "login"
          :> Auth '[BasicAuth, JWTWithSession] UserAuthenticated
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseLogin
              , WithStatus 401 ErrorBasicAuth
              , WithStatus 500 ()
              ]
  , routeLogout ::
      route
        :- Summary "Logout"
          :> Description
              "Logout from a user session.\n\
              \The token used with this request will be invalidated.\n"
          :> "logout"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseLogout
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  , routeRegister ::
      route
        :- Summary "Register"
          :> Description
              "Register a new user account.\n\
              \A confirmation email will be sent to the given email address.\n"
          :> "register"
          :> ReqBody' '[Lenient, Required] '[JSON] RequestRegister
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 201 ResponseRegister
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 409 (StaticText "Username is taken.")
              , WithStatus 500 ()
              ]
  , routePasswordChange ::
      route
        :- Summary "Change Password"
          :> Description
              "Set a new password for your user account.\n"
          :> "password"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestPasswordChange
          :> UVerb
              PATCH
              '[JSON]
              [ WithStatus 200 ResponsePasswordChange
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 500 ()
              ]
  , routeConfirm ::
      route
        :- Summary "Confirm"
          :> Description
              "Confirm your email address.\n"
          :> "confirm"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestConfirm
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseConfirm
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 410 ()
              , WithStatus 500 ()
              ]
  , routeProfile ::
      route
        :- Summary "View User"
          :> Description
              "Request detailed user information.\n"
          :> "profile"
          :> Auth '[JWTWithSession] UserAuthenticated
          :> ReqBody' '[Lenient, Required] '[JSON] RequestProfile
          :> UVerb
              POST
              '[JSON]
              [ WithStatus 200 ResponseProfile
              , WithStatus 400 ErrorParseBodyJson
              , WithStatus 401 ErrorBearerAuth
              , WithStatus 404 ()
              , WithStatus 500 ()
              ]
  }
  deriving stock (Generic)

type ResponseLogin :: Type
data ResponseLogin = MkResponseLogin
  { responseLoginJwt :: Jwt
  , responseLoginExpiration :: Maybe T.UTCTime
  , responseLoginId :: IdentifierUser
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseLogin") ResponseLogin

type Jwt :: Type
newtype Jwt = MkJwt {unJwt :: T.Text}
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving newtype (A.FromJSON, A.ToJSON)

type ResponseLogout :: Type
newtype ResponseLogout = MkResponseLogout
  { responseLogoutUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseLogout") ResponseLogout

type RequestRegister :: Type
data RequestRegister = MkRequestRegister
  { requestRegisterName :: Username
  , requestRegisterPassword :: Password
  , requestRegisterEmail :: EmailAddress
  , requestRegisterEmailVisible :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestRegister") RequestRegister

type ResponseRegister :: Type
newtype ResponseRegister = MkResponseRegister
  { responseRegisterEmailSent :: Bool
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseRegister") ResponseRegister

type RequestPasswordChange :: Type
newtype RequestPasswordChange = MkRequestPasswordChange
  { requestPasswordChangeNewPassword :: Password
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestPasswordChange") RequestPasswordChange

type ResponsePasswordChange :: Type
newtype ResponsePasswordChange = MkResponsePasswordChange
  { responsePasswordChangeUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responsePasswordChange") ResponsePasswordChange

type RequestConfirm :: Type
newtype RequestConfirm = MkRequestConfirm
  { requestConfirmSecret :: ConfirmationSecret
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestConfirm") RequestConfirm

type ResponseConfirm :: Type
newtype ResponseConfirm = MkResponseConfirm
  { responseConfirmUnit :: ()
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseConfirm") ResponseConfirm

type RequestProfile :: Type
newtype RequestProfile = MkRequestProfile
  { requestProfileUser :: NameOrIdentifier Username IdentifierUser
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkRequest" "requestProfile") RequestProfile

type ResponseProfile :: Type
data ResponseProfile = MkResponseProfile
  { responseProfileId :: IdentifierUser
  , responseProfileName :: Username
  , responseProfileEmail :: Maybe EmailAddress
  }
  deriving stock (Eq, Generic, Ord, Read, Show)
  deriving
    (A.FromJSON, A.ToJSON)
    via A.CustomJSON (JSONSettings "MkResponse" "responseProfile") ResponseProfile
