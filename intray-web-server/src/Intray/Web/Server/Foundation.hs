{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Web.Server.Foundation
    ( module Intray.Web.Server.Foundation
    , module Intray.Web.Server.Widget
    , module Intray.Web.Server.Static
    , module Intray.Web.Server.Constants
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T

import Control.Concurrent

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe

import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Media as Http
import qualified Network.HTTP.Types as Http
import Web.Cookie

import Text.Hamlet
import Yesod hiding (Header)
import Yesod.Auth
import qualified Yesod.Auth.Message as Msg
import Yesod.EmbeddedStatic

import Servant.API
import Servant.Auth.Client (Token(..))
import Servant.Client
import Servant.Common.Req

import Intray.Client

import Intray.Web.Server.Constants
import Intray.Web.Server.Persistence
import Intray.Web.Server.Static
import Intray.Web.Server.Widget

type IntrayWidget = IntrayWidget' ()

type IntrayWidget' = WidgetT App IO

type IntrayHandler = HandlerT App IO

type IntrayAuthHandler = HandlerT Auth IntrayHandler

data App = App
    { appHttpManager :: Http.Manager
    , appStatic :: EmbeddedStatic
    , appAPIBaseUrl :: BaseUrl
    , appPersistLogins :: Bool
    , appLoginTokens :: MVar (HashMap Username Token)
    }

mkYesodData "App" $(parseRoutesFile "routes")

instance Yesod App where
    defaultLayout widget = do
        pc <- widgetToPageContent $(widgetFile "default-body")
        withUrlRenderer $(hamletFile "templates/default-page.hamlet")
    yesodMiddleware = defaultCsrfMiddleware . defaultYesodMiddleware
    authRoute _ = Just $ AuthR LoginR

instance PathPiece Username where
    fromPathPiece = parseUsername
    toPathPiece = usernameText

instance YesodAuth App where
    type AuthId App = Username
    loginDest _ = AddR
    logoutDest _ = HomeR
    authHttpManager = appHttpManager
    authenticate creds =
        if credsPlugin creds == intrayAuthPluginName
            then case parseUsername $ credsIdent creds of
                     Nothing -> pure $ UserError Msg.InvalidLogin
                     Just un -> pure $ Authenticated un
            else pure $
                 ServerError $
                 T.unwords ["Unknown authentication plugin:", credsPlugin creds]
    authPlugins _ = [intrayAuthPlugin]
    maybeAuthId =
        runMaybeT $ do
            s <- MaybeT $ lookupSession credsKey
            MaybeT $ return $ fromPathPiece s

intrayAuthPluginName :: Text
intrayAuthPluginName = "intray-auth-plugin"

intrayAuthPlugin :: AuthPlugin App
intrayAuthPlugin = AuthPlugin intrayAuthPluginName dispatch loginWidget
  where
    dispatch "POST" ["login"] = postLoginR >>= sendResponse
    dispatch "GET" ["register"] = getNewAccountR >>= sendResponse
    dispatch "POST" ["register"] = postNewAccountR >>= sendResponse
    dispatch _ _ = notFound
    loginWidget :: (Route Auth -> Route App) -> IntrayWidget
    loginWidget _ = do
        token <- genToken
        msgs <- getMessages
        $(widgetFile "auth/login")

data LoginData = LoginData
    { loginUserkey :: Text
    , loginPassword :: Text
    } deriving (Show)

loginFormPostTargetR :: AuthRoute
loginFormPostTargetR = PluginR intrayAuthPluginName ["login"]

postLoginR :: IntrayAuthHandler TypedContent
postLoginR = do
    let loginInputForm =
            LoginData <$> ireq textField "userkey" <*>
            ireq passwordField "passphrase"
    result <- lift $ runInputPostResult loginInputForm
    muser <-
        case result of
            FormMissing -> invalidArgs ["Form is missing"]
            FormFailure _ -> return $ Left Msg.InvalidLogin
            FormSuccess (LoginData ukey pwd) ->
                case parseUsername ukey of
                    Nothing -> pure $ Left Msg.InvalidUsernamePass
                    Just un -> do
                        lift $
                            login
                                LoginForm
                                    { loginFormUsername = un
                                    , loginFormPassword = pwd
                                    }
                        pure $ Right un
    case muser of
        Left err -> loginErrorMessageI LoginR err
        Right un ->
            lift $
            setCredsRedirect $ Creds intrayAuthPluginName (usernameText un) []

registerR :: AuthRoute
registerR = PluginR intrayAuthPluginName ["register"]

getNewAccountR :: IntrayAuthHandler Html
getNewAccountR = do
    token <- genToken
    msgs <- getMessages
    lift $ defaultLayout $(widgetFile "auth/register")

data NewAccount = NewAccount
    { newAccountUsername :: Username
    , newAccountPassword1 :: Text
    , newAccountPassword2 :: Text
    } deriving (Show)

postNewAccountR :: IntrayAuthHandler TypedContent
postNewAccountR = do
    let newAccountInputForm =
            NewAccount <$>
            ireq
                (checkMMap
                     (\t ->
                          pure $
                          case parseUsernameWithError t of
                              Left err ->
                                  Left
                                      (T.pack $
                                       unwords
                                           [ "Invalid username:"
                                           , show t ++ ";"
                                           , err
                                           ])
                              Right un -> Right un)
                     usernameText
                     textField)
                "username" <*>
            ireq passwordField "passphrase" <*>
            ireq passwordField "passphrase-confirm"
    mr <- lift getMessageRender
    result <- lift $ runInputPostResult newAccountInputForm
    mdata <-
        case result of
            FormMissing -> invalidArgs ["Form is incomplete"]
            FormFailure msgs -> pure $ Left msgs
            FormSuccess d ->
                pure $
                if newAccountPassword1 d == newAccountPassword2 d
                    then Right
                             Registration
                                 { registrationUsername = newAccountUsername d
                                 , registrationPassword = newAccountPassword1 d
                                 }
                    else Left [mr Msg.PassMismatch]
    case mdata of
        Left errs -> do
            setMessage $ toHtml $ T.concat errs
            redirect registerR
        Right reg -> do
            errOrOk <- lift $ runClient $ clientRegister reg
            case errOrOk of
                Left err -> do
                    case err of
                        FailureResponse {} ->
                            case Http.statusCode $ responseStatus err of
                                409 ->
                                    setMessage
                                        "An account with this username already exists"
                                _ ->
                                    setMessage
                                        "Failed to register for unknown reasons."
                        _ ->
                            setMessage "Failed to register for unknown reasons."
                    redirect registerR
                Right NoContent ->
                    lift $ do
                        login
                            LoginForm
                                { loginFormUsername = registrationUsername reg
                                , loginFormPassword = registrationPassword reg
                                }
                        setCredsRedirect $
                            Creds
                                intrayAuthPluginName
                                (usernameText $ registrationUsername reg)
                                []

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance PathPiece (UUID a) where
    fromPathPiece = parseUUID
    toPathPiece = uuidText

withNavBar :: WidgetT App IO () -> HandlerT App IO Html
withNavBar widget = do
    mauth <- maybeAuthId
    msgs <- getMessages
    defaultLayout $(widgetFile "with-nav-bar")

genToken :: MonadHandler m => m Html
genToken = do
    req <- getRequest
    let tokenKey = defaultCsrfParamName
    pure $
        case reqToken req of
            Nothing -> mempty
            Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]

runClient :: ClientM a -> Handler (Either ServantError a)
runClient func = do
    man <- asks appHttpManager
    burl <- asks appAPIBaseUrl
    let cenv = ClientEnv man burl
    liftIO $ runClientM func cenv

runClientOrErr :: ClientM a -> Handler a
runClientOrErr func = do
    errOrRes <- runClient func
    case errOrRes of
        Left err ->
            handleStandardServantErrs err $ \u s m r ->
                error $ show (u, s, m, r) -- TODO deal with error
        Right r -> pure r

handleStandardServantErrs ::
       ServantError
    -> (UrlReq -> Http.Status -> Http.MediaType -> LB.ByteString -> Handler a)
    -> Handler a
handleStandardServantErrs err func =
    case err of
        FailureResponse urlReq status mediaType resp ->
            func urlReq status mediaType resp
        ConnectionError e -> redirect $ ErrorAPIDownR $ T.pack $ show e
        e -> error $ unwords ["Error while calling API:", show e]

login :: LoginForm -> Handler ()
login form = do
    errOrRes <- runClient $ clientLogin form
    case errOrRes of
        Left err ->
            handleStandardServantErrs err $ \urlReq status mediaType resp ->
                if status == Http.unauthorized401
                    then do
                        addMessage "error" "Unable to login"
                        redirect $ AuthR LoginR
                    else error $ show (urlReq, status, mediaType, resp)
        Right (Headers NoContent (HCons _ (HCons sessionHeader HNil))) ->
            case sessionHeader of
                Header session ->
                    recordLoginToken (loginFormUsername form) session
                _ -> undefined -- TODO deal with this error

withLogin :: (Token -> Handler Html) -> Handler Html
withLogin func = do
    un <- requireAuthId
    mLoginToken <- lookupToginToken un
    case mLoginToken of
        Nothing -> redirect $ AuthR LoginR
        Just token -> func token

lookupToginToken :: Username -> Handler (Maybe Token)
lookupToginToken un = do
    whenPersistLogins loadLogins
    tokenMapVar <- asks appLoginTokens
    tokenMap <- liftIO $ readMVar tokenMapVar
    pure $ HM.lookup un tokenMap

recordLoginToken :: Username -> SetCookie -> Handler ()
recordLoginToken un session = do
    let token = Token $ setCookieValue session
    tokenMapVar <- asks appLoginTokens
    liftIO $ modifyMVar_ tokenMapVar $ pure . HM.insert un token
    whenPersistLogins storeLogins

whenPersistLogins :: Handler () -> Handler ()
whenPersistLogins f = do
    b <- getsYesod appPersistLogins
    when b f

loadLogins :: Handler ()
loadLogins = do
    tokenMapVar <- asks appLoginTokens
    liftIO $ modifyMVar_ tokenMapVar $ \m -> fromMaybe m <$> readLogins

storeLogins :: Handler ()
storeLogins = do
    tokenMapVar <- asks appLoginTokens
    liftIO $ do
        m <- readMVar tokenMapVar
        writeLogins m
