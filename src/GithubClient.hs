{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module GithubClient where

import Data.Aeson
import Data.ByteString.UTF8 as BSU
import Data.Text (pack)
import Data.Text.Encoding
import Data.ByteString.Lazy

import Network.HTTP.Simple

import Import

import Data.Time.Clock.POSIX (getPOSIXTime)

import qualified Data.ByteString.Char8 as B

import Jose.Jwt
import Jose.Jwk
import Jose.Jwa

encodeAll :: Jwk -> String -> IO (Either JwtError Jwt)
encodeAll key appId = do
    posixNow <- getPOSIXTime
    let issuedAt = IntDate posixNow
    let expires = IntDate (posixNow + 9 * 60)
    let res = Jose.Jwt.encode [key] encoding payload where
        encoding = JwsEncoding RS256
        payload = Claims $ Data.ByteString.Lazy.toStrict $ Data.Aeson.encode $ JwtClaims
            { jwtIss = Just $ Data.Text.pack appId
            , jwtSub = Nothing
            , jwtAud = Nothing
            , jwtExp = Just expires
            , jwtNbf = Nothing
            , jwtIat = Just issuedAt
            , jwtJti = Nothing
            }
    res

generateAppJwtToken :: String -> IO (Either String String)
generateAppJwtToken appId = do
    privateKey <- B.readFile "config/github-app-private-key.jwk"
    let Just key = (Data.Aeson.decodeStrict privateKey :: Maybe Jwk)
    encoded <- encodeAll key appId

    return (case encoded of
                Right t -> Right $ BSU.toString $ unJwt t
                Left _ -> Left "Could not create a token")

data AccessTokenResponse = AccessTokenResponse
    { token :: String
    , expires_at :: String
    } deriving (Show, Generic)
instance FromJSON AccessTokenResponse

generateAccessToken :: String -> String -> String -> IO (Either String String)
generateAccessToken appId host installationId = do
    result <- generateAppJwtToken appId
    case result of
        Left _ -> return result
        Right jwt -> (do
            baseRequest <- parseRequest $ "POST " ++ host ++ "/app/installations/" ++ installationId ++ "/access_tokens"
            let request = setRequestHeader "User-Agent" ["NAVIKT Repo Tools"]
                        $ setRequestHeader "Accept" ["application/vnd.github.machine-man-preview+json"]
                        $ setRequestHeader "Authorization" [BSU.fromString $ "Bearer " ++ jwt]
                        $ baseRequest
            response <- httpJSON request :: IO (Response AccessTokenResponse)
            return $ Right $ token $ getResponseBody response)

makeGithubApiRequest :: (FromJSON m, ToJSON body) => App -> String -> String -> Maybe body -> IO (Either String (Response m))
makeGithubApiRequest app method request requestBody = do
    let appId = githubAppId $ appSettings app
        host = githubApiHost $ appSettings app
        installationId = githubInstallationId $ appSettings app
    maybeGithubToken <- generateAccessToken appId host installationId
    case maybeGithubToken of
        Right githubToken -> (do
            req <- parseRequest $ method ++ " " ++ host ++ "/" ++ request
            response <- httpJSON $ setRequestHeader "User-Agent" ["NAVIKT Repo Tools"]
                $ setRequestHeader "Accept" ["application/vnd.github.hellcat-preview+json"]
                $ setRequestHeader "Authorization" [BSU.fromString $ "token " ++ githubToken]
                $ (case requestBody of
                    Just body -> setRequestBodyJSON body
                    Nothing -> Import.id)
                $ req
            return $ Right response)
        Left error -> return $ Left error

makeEmptyResponseGithubApiRequest :: ToJSON body => App -> String -> String -> Maybe body -> IO (Either String (Response Data.ByteString.Lazy.ByteString))
makeEmptyResponseGithubApiRequest app method request requestBody = do
    let appId = githubAppId $ appSettings app
        host = githubApiHost $ appSettings app
        installationId = githubInstallationId $ appSettings app
    maybeGithubToken <- generateAccessToken appId host installationId
    case maybeGithubToken of
        Right githubToken -> (do
            req <- parseRequest $ method ++ " " ++ host ++ "/" ++ request
            response <- httpLBS $ setRequestHeader "User-Agent" ["NAVIKT Repo Tools"]
                $ setRequestHeader "Accept" ["application/vnd.github.hellcat-preview+json"]
                $ setRequestHeader "Authorization" [BSU.fromString $ "token " ++ githubToken]
                $ (case requestBody of
                    Just body -> setRequestBodyJSON body
                    Nothing -> Import.id)
                $ req
            return $ Right response)
        Left error -> return $ Left error
