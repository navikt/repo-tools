{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

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

loginUrl :: Route Auth
loginUrl = PluginR "oidc" ["forward"]
callbackUrl :: Route Auth
callbackUrl = PluginR "oidc" ["callback"]

authOidc :: YesodAuth master
    => O.OIDC
    -> AuthPlugin master
authOidc oidc =
    AuthPlugin "oidc" dispatch login
      where
        dispatch :: YesodAuth site
                 => Text
                 -> [Text]
                 -> AuthHandler site TypedContent
        dispatch "GET" ["forward"] = do
            -- app <- getYesod
            render <- getUrlRender
            let verUrl = render StaticR "/auth/page/oidc/callback" -- or use the callbackUrl definition from above?
            -- $logInfo verUrl
            url <- liftIO $ getAuthUrl oidc "http://localhost:3000/auth/page/oidc/callback" Nothing
            redirect $ urlToString url

        dispatch "POST" ["callback"] = do
            mgr <- authHttpManager
            errorCode <- lookupPostParam "error"
            case errorCode of
                Just ec -> (do
                    maybeMsg <- lookupPostParam "error_description"
                    $logError $ fromJust maybeMsg
                    url <- liftIO $ getAuthUrl oidc "http://localhost:3000/auth/page/oidc/callback" (Just "login")
                    redirect $ urlToString url)
                Nothing -> do
                    code <- lookupPostParam "code"
                    case code of
                        Just c -> do
                            let updatedOidc = oidc { S.oidcRedirectUri = "http://localhost:3000/auth/page/oidc/callback" }
                            tokens <- liftIO $ O.requestTokens updatedOidc (Data.Text.Encoding.encodeUtf8 c) mgr
                            -- let cl = claims $ idToken tokens
                            -- liftIO $ print tokens
                            -- liftIO $ print . O.claims . O.idToken $ tokens
                            -- $logInfo ((Data.Text.pack "Issuer: ") `Data.Text.append` (iss cl))
                            setCredsRedirect (Creds "oidc" "anonymous" [])
                        Nothing -> do
                            $logError "no ?code param found"
                            notFound

        dispatch _ _ = do notFound

        login authToMaster = toWidget [hamlet| <a href="@{authToMaster loginUrl}">Login here|]
