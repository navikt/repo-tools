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

module Handler.Github where

import Network.HTTP.Simple
import GithubClient

import Handler.Api

import Records.GithubAPIRepoPermissionRequest as RP
import Records.GithubAPICreateRepoRequest as G
import Records.GithubAPICreateRepoResponse as GR
import Records.APICreateRepoRequest as A
import Records.APICreateRepoResponse as AR

import Data.ByteString.Lazy

import Yesod
import Yesod.Auth
import Import

import Text.Read

data Team = Team
    { name :: String
    , id :: Int
    , slug :: String
    , description :: Maybe String
    } deriving (Show, Generic)
instance FromJSON Team
instance ToJSON Team

getMetaR :: Handler Value
getMetaR = do
    _ <- requireAuthId
    app <- getYesod
    result <- liftIO $ (makeGithubApiRequest app "GET" "rate_limit" (Nothing :: Maybe String) :: IO (Either String (Response Value)))
    case result of
        Right response -> do
            returnJson $ getResponseBody $ response
        Left _ -> returnJson $ JsonError $ "Oops"

getTeamsR :: Handler Value
getTeamsR = do
    _ <- requireAuthId
    app <- getYesod
    result <- liftIO $ (makeGithubApiRequest app "GET" "orgs/navikt/teams?per_page=100" (Nothing :: Maybe String) :: IO (Either String (Response [Team])))
    case result of
        Right response -> do
            returnJson $ getResponseBody $ response
        Left _ -> returnJson $ JsonError $ "Oops"

postCreateRepoR :: Handler Value
postCreateRepoR = do
    _ <- requireAuthId
    app <- getYesod
    body <- requireJsonBody :: Handler APICreateRepoRequest
    result <- liftIO $ (makeGithubApiRequest app "POST" "orgs/navikt/repos" (Just G.GithubAPICreateRepoRequest
      { G.name = A.title body
      , G.description = A.description body
      , private = False
      , has_projects = False
      , has_wiki = False
      , license_template = "mit"
      -- , team_id = read $ A.owner body
    }) :: IO (Either String (Response GithubAPICreateRepoResponse)))
    case result of
        Right response -> do
            let responseBody = getResponseBody response
                repoId = GR.id responseBody
                teamId = A.owner body
                fullName = GR.full_name responseBody
                url = "teams/" ++ teamId ++ "/repos/" ++ fullName

            result2 <- liftIO $ (makeEmptyResponseGithubApiRequest app "PUT" url (Just RP.GithubAPIRepoPermissionRequest
                { permission = "admin"
                }) :: IO (Either String (Response Data.ByteString.Lazy.ByteString)))
            case result2 of
                Right response2 -> do
                    $logInfo $ Import.pack $ show response2
                    returnJson AR.APICreateRepoResponse {
                        AR.message = "OK"
                    }
                Left _ -> returnJson $ JsonError $ "Could not assign owner"
        Left _ -> returnJson $ JsonError $ "Could not create repository"
