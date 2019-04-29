{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where
import Data.FileEmbed (embedFile)

import Import
import Yesod.Auth

-- getFallbackR :: Texts -> Handler ()
-- getFallbackR str = do
--    app <- getYesod
--    let indexPath = appStaticDir app </> "index.html"
--    sendFile "text/html" indexPath


getFallbackR :: Texts -> Handler TypedContent
getFallbackR _ = do
    _ <- requireAuthId
    return $ TypedContent typeHtml
        $ toContent $(embedFile "frontend/build/index.html")
