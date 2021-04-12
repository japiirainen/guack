import { flow } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { Lens } from '@effect-ts/monocle'
import * as Survey from '@guack/types'
import * as A from '@guack/types/ext/Array'
import { NonEmptyString } from '@guack/types/shared'

export const SurveyQuestion = Object.assign({}, Survey.SurveyQuestion, {
   create: (
      title: NonEmptyString,
      type: NonEmptyString,
      chs: Array<{ type: NonEmptyString; value: NonEmptyString }>
   ) =>
      Survey.SurveyQuestion.create({
         title,
         type,
         choices: chs.map(Survey.Choice.create),
      }),
})
export type SurveyQuestion = Survey.SurveyQuestion

const surveyQuestions = Survey.Survey.lens['|>'](Lens.prop('questions'))
const createAndAddQuestion = flow(SurveyQuestion.create, A.snoc)
const updateQuestionTitle = (q: SurveyQuestion) => (newTitle: NonEmptyString) =>
   A.modifyOrOriginal(q, q => ({ ...q, title: newTitle }))

const pastDate = (d: Date): O.Option<Date> => (d < new Date() ? O.some(d) : O.none)
