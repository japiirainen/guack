{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
module Domain.SurveyQuestionDomain
(
  SurveyQuestion (..)
, SurveyQuestionId (..)
, SurveyQuestionMap
, addQuestion
, deleteQuestion
, isQuestionPossible
)
where


import           Data.Aeson.Types    (FromJSON, ToJSON)
import qualified Data.List           as L
import qualified Data.Map.Strict     as M
import           Data.Time
import           Database.Persist
import           Database.Persist.TH
import           Domain.QuestionType
import           Domain.SurveyDomain
import           GHC.Generics


{--
This module implements the business logic for a single survey question
--}


-- | A data type + persistant SQL representation of a survey question
share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
SurveyQuestion
    title String                                -- ^ the title of the surveyQuestion
    surveyid SurveyId                           -- ^ the id of the parent survey
    type QuestionType                           -- ^ the QuestionType (see the module)
    choices [String] Maybe                      -- ^ ???
    created UTCTime default=CURRENT_TIME        -- ^ the created timestamp in UTC time
    deriving Show Eq Generic FromJSON ToJSON
|]


-- | a key value map holding a list of questions for a given survey
type SurveyQuestionMap = M.Map SurveyId [SurveyQuestion]


addQuestion :: SurveyQuestion -> [SurveyQuestion] -> [SurveyQuestion]
addQuestion q qs = q : qs

deleteQuestion :: SurveyQuestion-> [SurveyQuestion] -> [SurveyQuestion]
deleteQuestion = L.delete

isQuestionPossible :: SurveyQuestion -> [SurveyQuestion] -> Bool
isQuestionPossible (SurveyQuestion newTitle _ _ _ _) = not . any (\q -> surveyQuestionTitle q == newTitle)
