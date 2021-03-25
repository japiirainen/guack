import * as t from 'io-ts'
import { date } from 'io-ts-types'

export const Survey = t.type({
   id: t.number,
   active: t.boolean,
   startDate: date,
   expires: date,
   createdOn: date,
})

export const SurveyQuestion = t.type({
   id: t.number,
   title: t.string,
   surveyId: t.number,
   type: t.string,
   choices: t.array(t.string),
   createdOn: date,
})

export type Survey = t.TypeOf<typeof Survey>
export type SurveyQuestion = t.TypeOf<typeof SurveyQuestion>

export type QuestionType = 'CheckBox' | 'LinearScale' | 'MultiChoice' | 'TextField' | 'EmailField'
