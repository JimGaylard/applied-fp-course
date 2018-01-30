{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Bifunctor                     (first)
import           Data.Text                          (Text, unpack)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query),
                                                     close, open)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error (DBError), Topic,
                                                     fromDbComment, fromDbTopic,
                                                     getCommentText, getTopic)

import           FirstApp.DB.Types                  (DBComment, DBTopic)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
newtype FirstAppDB = FirstAppDB {conn :: Connection}

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB db = close $ conn db

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB
  :: FilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  c <- open fp
  Sql.execute_ c createTableQ
  return $ FirstAppDB c
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments
  :: FirstAppDB
  -> Topic
  -> IO (Either Error [Comment])
getComments db topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?" in
    do
      dbComments <- Sql.runDBAction $ Sql.query (conn db) sql (Sql.Only $ getTopic topic)
      return $ first DBError dbComments >>= traverse fromDbComment


addCommentToTopic
  :: FirstAppDB
  -> Topic
  -> CommentText
  -> IO (Either Error ())
addCommentToTopic db topic comment =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)" in
      do
        time <- getCurrentTime
        response <- Sql.runDBAction $ Sql.execute (conn db) sql
         (getTopic topic, getCommentText comment, time)
        return $ first DBError response

getTopics
  :: FirstAppDB
  -> IO (Either Error [Topic])
getTopics db =
  let
    sql = "SELECT DISTINCT topic FROM comments" in
    do
      dbTopics <- Sql.runDBAction $ Sql.query_ (conn db) sql
      return $ first DBError dbTopics >>= traverse fromDbTopic

deleteTopic
  :: FirstAppDB
  -> Topic
  -> IO (Either Error ())
deleteTopic db topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?" in
      do
        response <- Sql.runDBAction $ Sql.execute (conn db) sql (Sql.Only $ getTopic topic)
        return $ first DBError response

