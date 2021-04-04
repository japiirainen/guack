module UseCases.SurveyQuestionUseCase
(
  Dom.SurveyQuestion (..)
, Dom.SurveyQuestionId (..)
)
where

import           Colog.Polysemy              (Log, log)
import qualified Data.Map.Strict             as M
import           Domain.SurveyDomain         (SurveyId (..))
import qualified Domain.SurveyQuestionDomain as Dom (SurveyQuestion (..),
                                                     SurveyQuestionId (..),
                                                     SurveyQuestionMap,
                                                     addQuestion,
                                                     deleteQuestion,
                                                     isQuestionPossible)
import           Prelude                     hiding (log)

import           Data.Maybe                  (fromMaybe)
import           Numeric.Natural
import           Polysemy                    (Member, Sem)
import           Polysemy.Error
import           Polysemy.Input
import           UseCases.KVS                (KVS (..), deleteKvs, getKvs,
                                              insertKvs, listAllKvs)


type SurveyQuestionPersistence = KVS SurveyId [Dom.SurveyQuestion]

newtype SurveyQuestionError = QuestionNotPossible String

maxQuestions :: Natural
maxQuestions = 10

trySurveyQuestion :: (Member SurveyQuestionPersistence r, Member (Log String) r, Member (Error SurveyQuestionError) r) => Dom.SurveyQuestion -> Sem r ()
trySurveyQuestion question@(Dom.SurveyQuestion title sid _ _ _) = do
  log @String $ "trying to add " <> show title <> " to survey with id: " <> show sid
  questions <- fetchSurveyQuestions sid
  if Dom.isQuestionPossible question questions
    then persistQuestion question
    else throw $ QuestionNotPossible ("Question names need to be unique, " <> show title <> "is already used.")

  where
    persistQuestion :: (Member (KVS SurveyId [Dom.SurveyQuestion])r, Member (Log String) r) => Dom.SurveyQuestion -> Sem r ()
    persistQuestion q@(Dom.SurveyQuestion _ sid _ _ _) = do
      log @String $ "enter a new question to KVStore: " <> show q
      qs <- fetchSurveyQuestions sid
      let updated = Dom.addQuestion q qs
      log @String $ "storing: " <> show updated
      insertKvs sid updated



fetchSurveyQuestions :: (Member SurveyQuestionPersistence r, Member (Log String) r) => SurveyId -> Sem r [Dom.SurveyQuestion]
fetchSurveyQuestions sid = do
  log @String $ "fetch questions for survey with id: " <> show sid
  maybeList <- getKvs sid
  return $ fromMaybe [] maybeList


deleteQuestion :: (Member (KVS SurveyId [Dom.SurveyQuestion]) r, Member (Log String) r) => Dom.SurveyQuestion -> Sem r ()
deleteQuestion q@(Dom.SurveyQuestion _ sid _ _ _) = do
  log @String $ "deleting question with id: " <> show sid
  qs <- fetchSurveyQuestions sid
  log @String $ "before: " <> show qs
  let after = Dom.deleteQuestion q qs
  log @String $ "after: " <> show after
  insertKvs sid after


listAllSurveyQuestions :: (Member SurveyQuestionPersistence r, Member (Log String) r) => Sem r Dom.SurveyQuestionMap
listAllSurveyQuestions = do
  log @String $ "listing all questions"
  fmap M.fromList listAllKvs
