import * as O from '@effect-ts/core/Option'
import * as Lens from '@effect-ts/monocle/Lens'
import { AType, EType, make, opaque } from '@effect-ts/morphic'
import { CoreAlgebra } from '@effect-ts/morphic/Batteries/program'
import { BaseFC } from '@effect-ts/morphic/FastCheck/base'

import { SurveyQuestions } from './SurveyQuestion'
import { makeUuid, NonEmptyString } from './shared'

export const StartDate = make(F => F.nullable(F.date()))
export const ExpireDate = make(F => F.nullable(F.date()))
export const SurveyId = make(F => F.uuid())

export function EditableSurveyProps<Env extends BaseFC>(F: CoreAlgebra<'HKT', Env>) {
   return {
      title: NonEmptyString(F),
      active: F.boolean(),
      questions: SurveyQuestions(F),
   }
}

const Survey_ = make(F =>
   F.interface({
      id: SurveyId(F),
      expires: ExpireDate(F),
      startDate: StartDate(F),

      createdAt: F.date(),
      updatedAt: F.date(),

      ...EditableSurveyProps(F),
   })
)

export interface Survey extends AType<typeof Survey_> {}
export interface SurveyE extends EType<typeof Survey_> {}
const Survey0 = opaque<SurveyE, Survey>()(Survey_)

export const Survey = Object.assign(Survey0, {
   create: (a: Pick<Survey, 'title' | 'questions'>) => {
      const createdAt = new Date()
      return Survey.build({
         ...a,
         id: makeUuid(),
         createdAt,
         active: false,
         updatedAt: createdAt,
         startDate: O.none,
         expires: O.none,
      })
   },
   activate: Survey0.lens['|>'](Lens.prop('active')).set(true),
   close: Survey0.lens['|>'](Lens.prop('active')).set(false),
   setStartDate: Survey0.lens['|>'](Lens.prop('startDate')).set(O.some(new Date())),
   changeExpireDate: Survey0.lens['|>'](Lens.prop('expires')).set(O.some(new Date())),
})

// FE; surveylists contain surveys. BE; surveys have survey.listID
const SurveyListGroup_ = make(F =>
   F.interface({
      title: NonEmptyString(F),
      surveys: F.array(Survey(F)),
   })
)

export interface SurveyListGroup extends AType<typeof SurveyListGroup_> {}
export interface SurveyListGroupE extends EType<typeof SurveyListGroup_> {}
export const SurveyListGroup = opaque<SurveyListGroupE, SurveyListGroup>()(
   SurveyListGroup_
)

// In backend, surveys would be saved separately from lists and list separately from groups.
// so you would have a relationshipdb; task.listId, list.groupId etc.
