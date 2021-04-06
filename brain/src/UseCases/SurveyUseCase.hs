module UseCases.SurveyUseCase
(
  Survey (..)
, SurveyId (..)
, createSurvey
, fetchSurvey
, deleteSurvey
, SurveyError
, SurveyPersistence
)
where

import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Domain.SurveyDomain (Survey (..), SurveyId (..))
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input      ()
import           Polysemy.Trace      (Trace, trace)
import           UseCases.KVS        (KVS (..), deleteKvs, getKvs, insertKvs)


type SurveyPersistence = KVS SurveyId Survey

newtype SurveyError = SurveyNotFound String

createSurvey :: (Member SurveyPersistence r, Member Trace r) => SurveyId -> Survey -> Sem r ()
createSurvey sid survey = do
  trace $ "adding survey with id: " <> show sid
  insertKvs sid survey


fetchSurvey :: (Member (KVS SurveyId (Sem r Survey)) r, Member (Error SurveyError) r, Member Trace r) => SurveyId -> Sem r Survey
fetchSurvey sid = do
  trace $ "fetch survey with id: " <> show sid
  maybeSurvey <- getKvs sid
  case maybeSurvey of
    Nothing     -> throw $ SurveyNotFound ("survey not found, id: " <> show sid)
    Just survey -> survey

deleteSurvey :: (Member (KVS SurveyId Survey) r, Member Trace r) => SurveyId -> Sem r ()
deleteSurvey sid = do
  trace $ "deleting survey with id: " <> show sid
  deleteKvs sid
