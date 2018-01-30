{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Test.Hspec
import           Test.Hspec.Wai

import           Data.String                (fromString)

import qualified System.Exit                as Exit

import qualified Data.ByteString.Lazy.Char8 as LBS8

import qualified FirstApp.Main              as Main

main :: IO ()
main = do
  -- We need to setup our Application.
  let app' = pure Main.app

  -- This sets up HSpec to use our application as the thing it executes before the tests are run
  hspec . with app' $ do
      describe "List Route" $ do
        it "Should return a 'not implemented' message and 200 status" $
          get "/list" `shouldRespondWith` "List Request not implemented"
      describe "Add Route" $ do
        it "Should return a 'not implemented' message and 200 status" $
          post "/puppies/add" "foo" `shouldRespondWith` "Add Request not implemented"
        it "Should return an error with an empty comment" $
          post "/puppies/add" "" `shouldRespondWith` "Empty Comment" {matchStatus = 400}
      describe "View Route" $ do
        it "Should return a 'not implemented' when given a topic" $
          get "/puppies/view" `shouldRespondWith` "View Request not implemented"
        it "Should return an error when given an empty topic" $
          get "/view" `shouldRespondWith` "Unknown Route" {matchStatus = 404}
      describe "Bad Route" $ do
        it "Should return an error" $
          get "/apple" `shouldRespondWith` "Unknown Route" {matchStatus = 404}

      -- Write some more tests, below are some ideas to get you started:

      -- Don't worry if you don't get all of these done. :)

      -- 1) The '<topic>/add' route will respond with an error when given an empty comment
      -- 2) The '<topic>/view' route will respond correctly when given a topic
      -- 3) The '<topic>/view' route will respond with an error when given an empty topic
      -- 4) A gibberish route will return a 404

