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

module Records.GithubAPICreateRepoRequest where

import Import

data GithubAPICreateRepoRequest = GithubAPICreateRepoRequest
    { name :: String
    , description :: String
    , private :: Bool
    , has_projects :: Bool
    , has_wiki :: Bool
    , license_template :: String
    -- , team_id :: Int
    } deriving (Show, Generic)

instance ToJSON GithubAPICreateRepoRequest
