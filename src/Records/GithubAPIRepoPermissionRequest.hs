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

module Records.GithubAPIRepoPermissionRequest where

import Import

data GithubAPIRepoPermissionRequest = GithubAPIRepoPermissionRequest
    { permission :: String
    } deriving (Show, Generic)

instance ToJSON GithubAPIRepoPermissionRequest
