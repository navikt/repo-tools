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
import Records.GithubAPICreateFileRequest as GF
import Records.GithubAPICreateRepoResponse as GR
import Records.APICreateRepoRequest as A
import Records.APICreateRepoResponse as AR
import Records.GithubAPIGetTeamResponse as GGT

import qualified Data.ByteString as B
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.ByteString.Base64.Lazy as Base64

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

data Plan = Plan
    { filled_seats :: Int
    , seats :: Int
    } deriving (Show, Generic)
instance FromJSON Plan
instance ToJSON Plan

data Organization = Organization
    { plan :: Plan
    } deriving (Show, Generic)
instance FromJSON Organization
instance ToJSON Organization

getMetaR :: Handler Value
getMetaR = do
    app <- getYesod
    result <- liftIO $ (makeGithubApiRequest app "GET" "orgs/navikt" (Nothing :: Maybe String) :: IO (Either String (Response Organization)))
    case result of
        Right response -> do
            returnJson $ plan $ getResponseBody $ response
        Left _ -> returnJson $ JsonError $ "Oops"

getPrometheusR :: Handler TypedContent
getPrometheusR = do
    app <- getYesod
    result <- liftIO $ (makeGithubApiRequest app "GET" "orgs/navikt" (Nothing :: Maybe String) :: IO (Either String (Response Organization)))
    case result of
        Right response -> do
            let body = getResponseBody response
                filled = show $ filled_seats $ plan body
                total = show $ seats $ plan body
            return $ TypedContent typePlain
                    $ toContent ("github_seats{status=\"filled\",} " ++ filled ++ ".0\ngithub_seats{status=\"total\",} " ++ total ++ ".0\n" :: String)
        Left _ -> return $ TypedContent typePlain
                    $ toContent ("An error occured." :: String)

getTeamsR :: Handler Value
getTeamsR = do
    _ <- requireAuthId
    app <- getYesod
    result <- liftIO $ (makeGithubApiRequest app "GET" "orgs/navikt/teams?per_page=100" (Nothing :: Maybe String) :: IO (Either String (Response [Team])))
    case result of
        Right response -> do
            returnJson $ getResponseBody $ response
        Left _ -> returnJson $ JsonError $ "Oops"

codeowners :: String -> String
codeowners slug = "* @navikt/" ++ slug

base64Encode :: String -> String
base64Encode input = Char8.unpack $ Base64.encode $ Char8.pack input

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
                    result3 <- liftIO $ (makeGithubApiRequest app "GET" ("teams/" ++ A.owner body) (Nothing :: Maybe String) :: IO (Either String (Response GithubAPIGetTeamResponse)))
                    case result3 of
                        Right response3 -> do
                            _ <- liftIO $ (makeGithubApiRequest app "PUT" ("repos/" ++ fullName ++ "/contents/CODEOWNERS") (Just GF.GithubAPICreateFileRequest
                                 { GF.content = base64Encode $ codeowners $ GGT.slug $ getResponseBody response3
                                 , GF.message = "Add CODEOWNERS file"
                                 }) :: IO (Either String (Response Value)))
                            return ()

                    returnJson AR.APICreateRepoResponse {
                        AR.message = "OK"
                        , AR.html_url = GR.html_url responseBody
                    }
                Left _ -> returnJson $ JsonError $ "Could not assign owner"
        Left _ -> returnJson $ JsonError $ "Could not create repository"
