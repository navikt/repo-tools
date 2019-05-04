{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Auth.Oidc where

import Control.Monad.IO.Class

import Yesod
import Yesod.Auth
import Data.Maybe
import Data.Text
import Data.Text.Encoding
import Data.ByteString
import Network.URI

import qualified Web.OIDC.Client.Settings as S
import qualified Web.OIDC.Client as O

urlToString :: URI -> String
urlToString url = (uriToString id url) ""

getAuthUrl :: O.OIDC -> ByteString -> Maybe ByteString -> IO URI
getAuthUrl oidc redirectUrl prompt =
    O.getAuthenticationRequestUrl updatedOidc scopes state extraParams where
        updatedOidc = oidc { S.oidcRedirectUri = redirectUrl }
        scopes = [O.email, O.profile]
        state = Just ""
        extraParams = [("nonce", Just "somenonce"), ("response_mode", Just "form_post"), ("prompt", prompt)]

loginUrl :: AuthRoute
loginUrl = PluginR "oidc" ["forward"]
callbackUrl :: AuthRoute
callbackUrl = PluginR "oidc" ["callback"]

authOidc :: forall master. YesodAuth master
    => O.OIDC
    -> AuthPlugin master
authOidc oidc =
    AuthPlugin "oidc" dispatch login
      where
        dispatch
              :: ( MonadHandler m
                 , master ~ HandlerSite m
                 , Auth ~ SubHandlerSite m
                 )
              => Text
              -> [Text]
              -> m TypedContent
        dispatch "GET" ["forward"] = do
            r <- getUrlRender
            tm <- getRouteToParent
            let callbackEncoded = encodeUtf8 $ r $ tm $ callbackUrl

            url <- liftIO $ getAuthUrl oidc callbackEncoded Nothing
            redirect $ urlToString url

        dispatch "POST" ["callback"] = do
            mgr <- authHttpManager
            errorCode <- lookupPostParam "error"
            r <- getUrlRender
            tm <- getRouteToParent
            let callbackEncoded = encodeUtf8 $ r $ tm $ callbackUrl
            case errorCode of
                Just ec -> (do
                    maybeMsg <- lookupPostParam "error_description"
                    $logError $ fromJust maybeMsg
                    url <- liftIO $ getAuthUrl oidc callbackEncoded (Just "login")
                    redirect $ urlToString url)
                Nothing -> do
                    code <- lookupPostParam "code"
                    case code of
                        Just c -> do
                            let updatedOidc = oidc { S.oidcRedirectUri = callbackEncoded }
                            tokens <- liftIO $ O.requestTokens updatedOidc (Data.Text.Encoding.encodeUtf8 c) mgr
                            let cl = O.claims $ O.idToken tokens
                            -- $logInfo $ Data.Text.pack $ "tokens: " ++ show tokens
                            $logInfo "Got JWT token, the login was valid."
                            -- liftIO $ print tokens
                            -- liftIO $ print . O.claims . O.idToken $ tokens
                            -- $logInfo ((Data.Text.pack "Issuer: ") `Data.Text.append` (iss cl))
                            setCredsRedirect (Creds "oidc" "anonymous" [])
                        Nothing -> do
                            $logError "no ?code param found"
                            notFound

        dispatch _ _ = do notFound

        login authToMaster = toWidget [hamlet| <a href="@{authToMaster loginUrl}">Login here|]
