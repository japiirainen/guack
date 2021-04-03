module UseCases.SurveyUseCase
(
  Survey (..)
, SurveyId (..)
, trySurvey
, fetchSurvey
, deleteSurvey
)
where

import           Colog.Polysemy      (Log)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Domain.SurveyDomain (Survey (..), SurveyId (..))
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input      ()
import           Polysemy.KVStore    (KVStore (..))


type Persistence = KVStore SurveyId Survey

newtype SurveyError = SurveyNotPossible String


trySurvey :: (Member Persistence r, Member (Error SurveyError) r, Member (Log String) r) => SurveyId -> Survey -> Sem r ()
trySurvey = undefined

fetchSurvey :: (Member Persistence r, Member (Log String) r) => SurveyId -> Sem r Survey
fetchSurvey = undefined

deleteSurvey :: (Member (KVStore SurveyId Survey) r, Member (Log String) r) => SurveyId -> Sem r ()
deleteSurvey = undefined
