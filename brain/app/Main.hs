{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Main where

import           Control.Monad.IO.Class  (liftIO)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
BlogPost
    title String
    authorId PersonId
    deriving Show
|]

main :: IO ()
main = runSqlite ":memory:" $ do
    runMigration migrateAll

    joonaId <- insert $ Person "Joona Piirainen" $ Just 22
    jaanaId <- insert $ Person "Jaana" $ Nothing

    insert $ BlogPost "First blog post" joonaId
    insert $ BlogPost "Second blog post" jaanaId

    oneJoonaPost <- selectList [BlogPostAuthorId ==. joonaId] [LimitTo 1]
    liftIO $ print (oneJoonaPost :: [Entity BlogPost])

    joona <- get joonaId
    liftIO $ print joona

    delete jaanaId
    deleteWhere [BlogPostAuthorId ==. joonaId]

