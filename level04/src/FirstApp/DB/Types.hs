{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.DB.Types
  (DBComment(dbId, dbTopic, dbBody, dbTime),
   DBTopic(dbTopicId)
  ) where

import           Data.Text                      (Text)
import           Data.Time                      (UTCTime)

import           Database.SQLite.Simple.FromRow (FromRow (fromRow), field)

-- To try to avoid leaking various types and expected functionality around the
-- application, we create a stand alone type that will represent the data we
-- store in the database. In this instance, it is the raw types that make up a
-- comment.
--
-- Complete in the DbComment type below so it is a record type that matches the
-- Comment type, but without the newtype wrappers for each value.
data DBComment = DBComment
  { dbId    :: Int
  , dbTopic :: Text
  , dbBody  :: Text
  , dbTime  :: UTCTime
  }
  deriving Show

-- This Typeclass comes from the `sqlite-simple` package and describes how to
-- decode a single row from the database into a single representation of our
-- type. This technique of translating a result row to a type will differ
-- between different packages/databases.
instance FromRow DBComment where
  fromRow = DBComment <$> field <*> field <*> field <*> field

-- Now move to ``src/FirstApp/Types.hs``

newtype DBTopic = DBTopic { dbTopicId :: Text } deriving Show

instance FromRow DBTopic where
  fromRow = DBTopic <$> field
