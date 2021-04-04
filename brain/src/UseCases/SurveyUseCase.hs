module UseCases.SurveyUseCase
(
  Survey (..)
, SurveyId (..)
, createSurvey
)
where

import           Colog.Polysemy      (Log, log)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe)
import           Domain.SurveyDomain (Survey (..), SurveyId (..))
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input      ()
import           Prelude             hiding (log)
import           UseCases.KVS        (KVS (..), deleteKvs, getKvs, insertKvs)


type SurveyPersistence = KVS SurveyId Survey

newtype SurveyError = SurveyNotFound String


createSurvey :: (Member SurveyPersistence r, Member (Log String) r) => SurveyId -> Survey -> Sem r ()
createSurvey sid survey = do
  log @String $ "adding survey with id: " <> show sid
  insertKvs sid survey


fetchSurvey :: (Member SurveyPersistence r, Member (Error SurveyError) r, Member (Log String) r) => SurveyId -> Sem r (Maybe Survey)
fetchSurvey sid = do
  log @String $ "fetch survey with id: " <> show sid
  maybeSurvey <- getKvs sid
  case maybeSurvey of
    Nothing     -> throw $ SurveyNotFound ("survey not found, id: " <> show sid)
    Just survey -> return $ Just survey

deleteSurvey :: (Member (KVS SurveyId Survey) r, Member (Log String) r) => SurveyId -> Sem r ()
deleteSurvey sid = do
  log @String $ "deleting survey with id: " <> show sid
  deleteKvs sid

