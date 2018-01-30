{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               (Last (Last), (<>))

import           FirstApp.Types            (Conf (Conf), ConfigError (DBPathConfigMissing, PortConfigMissing),
                                            DBFilePath (DBFilePath),
                                            PartialConf (PartialConf),
                                            Port (Port))

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf
  :: PartialConf
defaultConf =
   PartialConf
   (Last (Just (Port 8080)))
   (Last (Just (DBFilePath "appconfig.json")))

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig
  :: PartialConf
  -> Either ConfigError Conf
makeConfig (PartialConf (Last (Just port)) (Last (Just dbPath))) = Right (Conf port dbPath)
makeConfig (PartialConf (Last Nothing) _) = Left PortConfigMissing
makeConfig (PartialConf _ (Last Nothing)) = Left DBPathConfigMissing

-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions
  :: FilePath
  -> IO (Either ConfigError Conf)
parseOptions fp = do
  -- Parse the options from the config file: "appconfig.json"
  -- Parse the options from the commandline using 'commandLineParser'
  -- Combine these with the default configuration 'defaultConf'
  -- Return the final configuration value
  errOrFileOpts <- parseJSONConfigFile fp
  cmd <- commandLineParser
  let combine fc = defaultConf <> fc <> cmd
  return $ fmap combine errOrFileOpts >>= makeConfig
