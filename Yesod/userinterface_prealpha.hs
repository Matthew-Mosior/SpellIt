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
    rootwords Text
    partsofspeech Text
    rootforms Text
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
getHomeR = defaultLayout $ do
    setTitle "Spellit Pre-Alpha"
    addScriptRemote 
        "https://ajax.googleapis.com/ajax/libs/jquery/3.3.1/jquery.min.js"
    toWidget
        [julius|
         $(function()
         { 
                //Counter variables.
                var pofscounter = 2;
                var rfcounter = 2;
    
                //Part of Speech Button.
                //Add the button.
                $("#partofspeechbutton").click(function()
                {
                    var partofspeechtbox = $(document.createElement('div'))
                                           .attr("id" , "POSDiv" + pofscounter);

                    partofspeechtbox.after().html('<label>Part Of Speech # '+ pofscounter + ' : </label>' + 
                                            '<input type=text name="text"' + pofscounter + 
                                            ' id="partofspeechtbox"' + pofscounter + ' value="">');
                    
                    partofspeechtbox.appendTo("#PartOfSpeechBoxesGroup");

                    pofscounter++;  
                });
                
                //Remove the button.
                $("#removepartofspeechbutton").click(function()
                {
                    if(pofscounter == 1)
                    {
                        alert("No more Part of Speech Textboxes to remove.");
                        return false;
                    }

                    pofscounter--;
                       
                    $("#POSDiv" + pofscounter).remove();
                });
                //
                
                //Root Forms Button.
                //Add the button.
                $("#rootformsbutton").click(function()
                {
                    var rootformstbox = $(document.createElement('div'))
                                        .attr("id" , "RFDiv" + rfcounter);
                    
                    rootformstbox.after().html('<label>Root Forms#' + rfcounter + ' : </label>' +
                        '<input type=text name="text"' + rfcounter + 
                        ' id="rootformstbox"' + rfcounter + ' value="" >');
                   
                    rootformstbox.appendTo("#RootFormsBoxesGroup");
                    
                    rfcounter++; 
                });

                //Remove the button.
                $("#removerootformsbutton").click(function()
                {
                    if(rfcounter == 1)
                    {
                        alert("No more Root Form Textboxes to remove.");
                        return false;
                    }

                    rfcounter--;

                    $("#RFDiv" + rfcounter).remove();
                });
                //
            });
        |]

    toWidget
        [lucius|
            * {
                 box-sizing: border-box;
             }
              
             .column 
             {
                 float:   left;
                 width:   33.33%
                 padding: 10px;
             }

             .row:after
             {
                 content: "";
                 display: table;
                 clear:   both;
             }
             
             @media screen and (max-width: 600 px)
             {
                 .column
                 {
                     width: 100%;
                 }
             }
        |]                  
    [whamlet|
        <form method=post action=@{AddInfoR}>
        <h1>Spellit Pre-Alpha
        <p>
            Add Language
            <input type=text name=lang>
        <p>
        <p>
            New Root Word
            <input type=button value="Add Root Word" id="newrootword">
        <p>
        <div class="row">
            <div class="column" style="background-color:#aaa;">
                <h2>Root Word
                <input type=text name=rw>
            <div class="column" style="background-color:#bbb;">
                <h2>Parts of Speech
                <p>
                    <div id=PartOfSpeechBoxesGroup>
                        <div id=POSDiv1>
                            <label>Part of Speech #1<input type=text name=textbox id=partofspeechtbox1 value="">
                <input type=button value="Add Part of Speech" id=partofspeechbutton>
                <input type=button value="Remove Part of Speech" id=removepartofspeechbutton>  
            <div class="column" style="background-color:#ccc;">
                <h2>Root Forms
                <p>
                    <div id=RootFormsBoxesGroup>
                        <div id=RFDiv1>
                            <label>Root Form #1<input type=text name=textbox id=rootformstbox1 value="">
                <input type=button value="Add Root Form" id=rootformsbutton>
                <input type=button value="Remove Root Form" id=removerootformsbutton>
        <p>
        <p>
            <input type=submit value="Create Affix File"> 
    |]

postAddInfoR :: Handler ()
postAddInfoR = do
    languagein <- runInputPost $ ireq textField "lang"
    rootwordsin <- runInputPost $ ireq textField "rw" 
    partsofspeechin <- runInputPost $ ireq textField "pos" 
    rootformsin <- runInputPost $ ireq textField "rf"
    now <- liftIO getCurrentTime
    runDB $ insert $ Spellit languagein rootwordsin partsofspeechin rootformsin now
    setMessage "Submitted for affix file generation."
    redirect HomeR

main :: IO ()
main = runNoLoggingT $ withSqlitePool "Spellit.db3" 10 $ \pool -> liftIO $ do
    runSqlPersistMPool (runMigration migrateAll) pool
    warp 3000 $ App pool 
