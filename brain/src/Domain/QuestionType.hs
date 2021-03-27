{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}
module Domain.QuestionType
(QuestionType (..))
where

import           Data.Aeson.Types    (FromJSON, ToJSON)
import           Database.Persist.TH
import           GHC.Generics

data QuestionType =   CheckBox
                    | LinearScale
                    | MultiChoice
                    | TextField
                    | EmailField
    deriving (Show, Read, Eq, Generic, FromJSON, ToJSON)
derivePersistField "QuestionType"


