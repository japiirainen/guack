import { flow } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { Lens } from '@effect-ts/monocle'
import * as S from '@guack/types'
import * as A from '@guack/types/ext/Array'
import { NonEmptyString } from '@guack/types/shared'

type NewChoice = { type: NonEmptyString; value: NonEmptyString }
export const SurveyQuestion = Object.assign({}, S.SurveyQuestion, {
   create: (title: NonEmptyString, type: NonEmptyString, chs: Array<NewChoice>) =>
      S.SurveyQuestion.create({
         title,
         type,
         choices: chs.map(S.Choice.create),
      }),
})
export type SurveyQuestion = S.SurveyQuestion

const surveyQuestions = S.Survey.lens['|>'](Lens.prop('questions'))
const createAndAddQuestion = flow(SurveyQuestion.create, A.snoc)
const updateQuestionTitle = (q: SurveyQuestion) => (newTitle: NonEmptyString) =>
   A.modifyOrOriginal(q, q => ({ ...q, title: newTitle }))

const pastDate = (d: Date): O.Option<Date> => (d < new Date() ? O.some(d) : O.none)

export function updateSurveyIndex(s: Survey, newIndex: number) {
   return (surveys: A.Array<Survey>) => {
      const modifiedSurveys = surveys['|>'](A.filter(x => x !== s))['|>'](
         A.insertAt(newIndex, s)
      )
      return modifiedSurveys['|>'](O.getOrElse(() => surveys))
   }
}

export function updateQuestionIndex(q: SurveyQuestion, newIndex: number) {
   return (qs: A.Array<SurveyQuestion>) => {
      const modifiedQs = qs['|>'](A.filter(x => x !== q))['|>'](A.insertAt(newIndex, q))
      return modifiedQs['|>'](O.getOrElse(() => qs))
   }
}

export const Survey = Object.assign({}, S.Survey, {
   addQuestion: (title: NonEmptyString, type: NonEmptyString, chs: Array<NewChoice>) =>
      surveyQuestions['|>'](Lens.modify(createAndAddQuestion(title, type, chs))),
   deleteQuestion: (q: SurveyQuestion) =>
      surveyQuestions['|>'](Lens.modify(A.deleteOrOriginal(q))),
   updateQuestion: (q: S.SurveyQuestion, questionTitle: NonEmptyString) =>
      surveyQuestions['|>'](Lens.modify(updateQuestionTitle(q)(questionTitle))),
   updateQuestionIndex: (s: S.SurveyQuestion, newIndex: number) =>
      surveyQuestions['|>'](Lens.modify(updateQuestionIndex(s, newIndex))),
   expiresInPast: flow(S.Survey.lens['|>'](Lens.prop('expires')).get, O.chain(pastDate)),
   startDateInPast: flow(
      S.Survey.lens['|>'](Lens.prop('startDate')).get,
      O.chain(pastDate)
   ),
})

export type Survey = S.Survey

export * from '@guack/types/Survey'
