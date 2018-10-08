{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

{-Imports.-}

import Control.Monad.Logger
import Data.Text
import Data.Time
import Database.Persist.Sqlite
import Yesod

{----------}

share [mkPersist sqlSettings , mkMigrate "migrateAll"] [persistLowerCase|
Spellit 
    language Text
    added UTCTime
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/ HomeR            GET
/add-info AddInfoR POST
|]

instance Yesod App

instance RenderMessage App FormMessage
    where
        renderMessage _ _ = defaultFormMessage

instance YesodPersist App 
    where
        type YesodPersistBackend App = SqlBackend
        runDB db = do
            App pool <- getYesod
            runSqlPool db pool

getHomeR :: Handler Html
getHomeR = defaultLayout
    [whamlet|
        <form method=post action=@{AddInfoR}>
            <p>
                Add language:
                <input type=text name=lang>
                <input type=submit value="Add language">
       <h2>Current Language and Paradigm:
       ^{existingInfo}
    |]

existingInfo :: Widget
existingInfo = do
    inputs <- handlerToWidget $ runDB $ selectList [] [Asc SpellitAdded]
    [whamlet|
        <ul>
            $forall Entity _ input <- inputs
                <li>#{spellitLanguage input}
                    
    |]

postAddInfoR :: Handler ()
postAddInfoR = do
    language <- runInputPost $ ireq textField "lang"
    now <- liftIO getCurrentTime
    runDB $ insert $ Spellit language now
    setMessage "Language Added"
    redirect HomeR

main :: IO ()
main = runNoLoggingT $ withSqlitePool "links.db3" 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ App pool 
