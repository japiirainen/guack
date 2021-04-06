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
import qualified Polysemy.Input                 as PI
import           Polysemy.Internal.Union        (Member)
import           Prelude                        hiding (log)
import           UseCases.KVS                   (KVS (..))

data KeyValueRow = KeyValueRow T.Text T.Text
    deriving (Show)

instance FromRow KeyValueRow where
    fromRow = KeyValueRow <$> field <*> field


runKvsAsSQLite :: (Member (Embed IO) r, Member (PI.Input Config) r, Member (Log String) r, Show k, Read k, ToJSON v, FromJSON v)
                    => Sem (KVS k v : r) a
                    -> Sem r a
runKvsAsSQLite = interpret $ \case
    GetKvs k      -> getAction k
    ListAllKvs    -> listAction
    InsertKvs k v -> insertAction k v
    DeleteKvs   k -> deleteAction k

    where
        getAction :: (Member (PI.Input Config) r, Member (Embed IO) r, Member (Log String) r, Show k, FromJSON v) => k -> Sem r (Maybe v)
        getAction key = do
            log @String $ "getAction: " ++ show key
            conn <-  PI.input input
            rows <- embed (SQL.queryNamed conn
                                "SELECT key, value FROM store WHERE key = :key"
                                [":key" := show key] :: IO [KeyValueRow])
            case rows of
                []                         -> return Nothing
                (KeyValueRow _key value):_ -> return $ (decode . encodeUtf8) value

        listAction :: (Member (PI.Input Config) r, Member (Embed IO) r, Member (Log String) r, Show k, ToJSON v) => k -> v -> Sem r ()
        listAction = undefied

        inserAction = undefined

        deleteAction = undefined

