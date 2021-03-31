module UseCases.SurveyUseCase
(
  Survey (..)
, SurveyId (..)
, SurveyQuestion (..)
, SurveyQuestionId (..)
)
where

import qualified Data.Map.Strict             as M
import           Data.Maybe                  (fromMaybe)
import           Domain.SurveyDomain         (Survey (..), SurveyId (..))
import           Domain.SurveyQuestionDomain (SurveyQuestion (..),
                                              SurveyQuestionId (..))
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input              ()
import           Polysemy.KVStore            (KVStore (..))
import           Polysemy.Trace              (Trace, trace)


type Persistence = KVStore SurveyId [SurveyQuestion]

newtype SurveyError = SurveyNotPossible String


maxQuestions :: Natural
maxQuestions = 20

trySurvey :: (Member Persistence r, Member (Error SurveyError) r, Member Trace r) => SurveyId -> [SurveyQuestion] -> Sem r ()
trySurvey = undefined

fetchQuestions :: (Member Persistence r, Member Trace r) => SurveyId -> Sem r [SurveyQuestion]
fetchQuestions = undefined

deleteQuestion :: (Member (KVStore SurveyId [SurveyQuestion]) r, Member Trace r) => SurveyQuestion -> Sem r ()
deleteQuestion = undefined
