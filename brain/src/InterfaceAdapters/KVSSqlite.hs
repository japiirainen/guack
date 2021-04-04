{-# LANGUAGE OverloadedStrings #-}
module InterfaceAdapters.KVSSqlite where

import           Colog.Polysemy                 (Log, log)
import           Data.Aeson                     (decode, encode)
import           Data.Aeson.Types               (FromJSON, ToJSON)
import           Data.Text.Lazy                 as T
import           Data.Text.Lazy.Encoding
import           Database.SQLite.Simple         (NamedParam ((:=)))
import           Database.SQLite.Simple         as SQL
import           Database.SQLite.Simple.FromRow
import           Database.SQLite.Simple.ToRow
import           InterfaceAdapters.Config
import           Polysemy
import           Polysemy.Input
import           Polysemy.Internal.Union        (Member)
import           Polysemy.KVStore               (KVStore (..))

data KeyValueRow = KeyValueRow T.Text T.Text
    deriving (Show)

instance FromRow KeyValueRow where
    fromRow = KeyValueRow <$> field <*> field


runKvsAsSQLite :: (Member (Embed IO) r, Member (Input Config) r, Member (Log String) r, Show k, Read k, ToJSON v, FromJSON v)
                    => Sem (KVStore k v : r) a
                    -> Sem r a
runKvsAsSQLite = undefined

