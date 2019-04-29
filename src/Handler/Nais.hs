module Handler.Nais where

import Yesod.Auth
import Import

getIsAliveR :: Handler TypedContent
getIsAliveR = return $ TypedContent typePlain
                    $ toContent "yes i am alive"

getIsReadyR :: Handler TypedContent
getIsReadyR = return $ TypedContent typePlain
                    $ toContent "yes i am ready"

getMeR :: Handler TypedContent
getMeR = do
    aid <- requireAuthId
    let msg = "Hello, " Prelude.++ (unpack aid)
    return $ TypedContent typePlain $ toContent msg

