{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf
    ( Conf (..)
    , firstAppConfig
    ) where

newtype Conf = Conf
  { dbFilePath :: FilePath
  }

firstAppConfig :: Conf
firstAppConfig = Conf "app_db.db"
