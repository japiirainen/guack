import * as Lens from '@effect-ts/monocle/Lens'
import { AType, EType, make, opaque } from '@effect-ts/morphic'

import { makeUuid, NonEmptyString } from './shared'
import { SurveyId } from './Survey'

// export const SurveyQuestion = t.type({
//    id: t.number,
//    title: t.string,
//    surveyId: t.number,
//    type: t.string,
//    choices: t.array(t.string),
//    createdOn: date,
// })

export const Choice_ = make(F =>
   F.interface({
      type: NonEmptyString(F),
      required: F.boolean(),
      value: NonEmptyString(F),
   })
)
export interface Choice extends AType<typeof Choice_> {}
export interface ChoiceE extends EType<typeof Choice_> {}
const Choice0 = opaque<ChoiceE, Choice>()(Choice_)
export const Choice = Object.assign(Choice_, {
   create: (a: Omit<Choice, 'required'>) => Choice.build({ ...a, required: false }),
   require: Choice0.lens['|>'](Lens.prop('required')).set(true),
})

export const Choices = make(F => F.array(Choice(F)))

const SurveyQuestion_ = make(F =>
   F.interface({
      id: F.uuid(),
      title: NonEmptyString(F),
      surveyId: SurveyId(F),
      type: NonEmptyString(F),
      choices: Choices(F),
      createdAt: F.date(),
      updatedAt: F.date(),
   })
)

export interface SurveyQuestion extends AType<typeof SurveyQuestion_> {}
export interface SurveyQuestionE extends EType<typeof SurveyQuestion_> {}
export const SurveyQuestion0 = opaque<SurveyQuestionE, SurveyQuestion>()(SurveyQuestion_)
export const SurveyQuestion = Object.assign(SurveyQuestion_, {
   create: (a: Omit<SurveyQuestion, 'id' | 'createdAt' | 'updatedAt'>) => {
      const createdAt = new Date()
      SurveyQuestion.build({
         ...a,
         id: makeUuid(),
         createdAt,
         updatedAt: createdAt,
      })
   },
})
