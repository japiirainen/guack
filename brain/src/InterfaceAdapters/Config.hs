module InterfaceAdapters.Config where


data Config = Config
    { port    :: String     -- ^ the port where the server is listening
    , backend :: Backend    -- ^ selects the persistence backend for the KV store
    , dbPath  :: String     -- ^ the path to the database
    , verbose :: Bool }     -- ^ True enables logging

data Backend = Postgres | SQLite deriving (Eq, Show)
