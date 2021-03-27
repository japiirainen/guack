{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Domain.SurveyDomain where

import           Data.Aeson.Types    (FromJSON, ToJSON)
import           Data.Time
import           Database.Persist
import           Database.Persist.TH
import           GHC.Generics


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Survey
    active Bool
    start_date UTCTime
    expires UTCTime
    created UTCTime default=CURRENT_TIME
    deriving Show Generic FromJSON ToJSON
|]
