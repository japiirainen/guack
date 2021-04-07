import * as O from '@effect-ts/core/Option'
import * as Lens from '@effect-ts/monocle/Lens'
import { AType, EType, make, opaque } from '@effect-ts/morphic'

import { makeUuid, NonEmptyString } from './shared'

export const StartDate = make(F => F.nullable(F.date()))
export const SurveyId = make(F => F.uuid())

const Survey_ = make(F =>
   F.interface({
      id: SurveyId(F),
      title: NonEmptyString(F),
      active: F.boolean(),
      expires: F.date(),
      createdAt: F.date(),
      updatedAt: F.date(),
      startDate: StartDate(F),
   })
)

export interface Survey extends AType<typeof Survey_> {}
export interface SurveyE extends EType<typeof Survey_> {}
const Survey0 = opaque<SurveyE, Survey>()(Survey_)

export const Survey = Object.assign(Survey0, {
   create: (a: Omit<Survey, 'id' | 'active' | 'startDate' | 'createdAt' | 'updatedAt'>) => {
      const createdAt = new Date()
      return Survey.build({
         ...a,
         id: makeUuid(),
         createdAt,
         active: false,
         updatedAt: createdAt,
         startDate: O.none,
      })
   },
   activate: Survey0.lens['|>'](Lens.prop('active')).set(true),
   close: Survey0.lens['|>'](Lens.prop('active')).set(false),
   setStartDate: Survey0.lens['|>'](Lens.prop('startDate')).set(O.some(new Date())),
})
