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

module Records.GithubAPICreateFileRequest where

import Import

data GithubAPICreateFileRequest = GithubAPICreateFileRequest
    { message :: String
    , content :: String
    } deriving (Show, Generic)

instance ToJSON GithubAPICreateFileRequest
