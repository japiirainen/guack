module UseCases.SurveyQuestionUseCase
(
  Dom.SurveyQuestion (..)
, Dom.SurveyQuestionId (..)
)
where

import           Colog.Polysemy              (Log, log)
import           Domain.SurveyDomain         (SurveyId (..))
import qualified Domain.SurveyQuestionDomain as Dom (SurveyQuestion (..),
                                                     SurveyQuestionId (..),
                                                     addQuestion,
                                                     deleteQuestion,
                                                     isQuestionPossible)
import           Prelude                     hiding (log)

import           Data.Maybe                  (fromMaybe)
import           Numeric.Natural
import           Polysemy                    (Member, Sem)
import           Polysemy.Error
import           Polysemy.Input
import           Polysemy.KVStore            (KVStore (..), lookupKV, writeKV)


type SurveyQuestionPersistence = KVStore SurveyId [Dom.SurveyQuestion]

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
    persistQuestion :: (Member (KVStore SurveyId [Dom.SurveyQuestion])r, Member (Log String) r) => Dom.SurveyQuestion -> Sem r ()
    persistQuestion q@(Dom.SurveyQuestion _ sid _ _ _) = do
      log @String $ "enter a new question to KVStore: " <> show q
      qs <- fetchSurveyQuestions sid
      let updated = Dom.addQuestion q qs
      log @String $ "storing: " <> show updated
      writeKV sid updated



fetchSurveyQuestions :: (Member SurveyQuestionPersistence r, Member (Log String) r) => SurveyId -> Sem r [Dom.SurveyQuestion]
fetchSurveyQuestions sid = do
  log @String $ "fetch questions for survey with id: " <> show sid
  maybeList <- lookupKV sid
  return $ fromMaybe [] maybeList


deleteQuestion :: (Member (KVStore SurveyId [Dom.SurveyQuestion]) r, Member (Log String) r) => Dom.SurveyQuestion -> Sem r ()
deleteQuestion q@(Dom.SurveyQuestion _ sid _ _ _) = do
  log @String $ "deleting question with id: " <> show sid
  qs <- fetchSurveyQuestions sid
  log @String $ "before: " <> show qs
  let after = Dom.deleteQuestion q qs
  log @String $ "after: " <> show after
  writeKV sid after
