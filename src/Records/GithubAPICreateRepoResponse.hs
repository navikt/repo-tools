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

module Records.GithubAPICreateRepoResponse where

import Import

-- Lots of other fields as well in the response, but this is what we need for now
data GithubAPICreateRepoResponse = GithubAPICreateRepoResponse
    { id :: Int
    , full_name:: String
    } deriving (Show, Generic)

instance FromJSON GithubAPICreateRepoResponse
