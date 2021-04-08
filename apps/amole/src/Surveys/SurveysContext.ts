import * as A from '@effect-ts/core/Array'
import * as T from '@effect-ts/core/Effect'
import { flow, pipe } from '@effect-ts/core/Function'
import * as Map from '@effect-ts/core/Map'
import * as O from '@effect-ts/core/Option'
import * as Sy from '@effect-ts/core/Sync'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import { encode } from '@effect-ts/morphic/Encoder'
import { strict } from '@effect-ts/morphic/Strict'
import { strictDecoder } from '@effect-ts/morphic/StrictDecoder'
import { Survey, SurveyE, SurveyQuestion, Choice } from '@guack/types'
import * as EO from '@guack/types/ext/EffectOption'
import { NonEmptyString } from '@guack/types/shared'

import { NotFoundError } from '../infrastrucure/errors'

const encodeSurvey = flow(strict(Survey).shrink, Sy.chain(encode(Survey)))
const runEncodeSurvey = flow(encodeSurvey, Sy.run)
let surveys: Map.Map<UUID, SurveyE> = pipe(
   [
      Survey.create({
         title: 'First survey' as NonEmptyString,
         questions: [
            SurveyQuestion.create({
               title: 'q1' as NonEmptyString,
               choices: [
                  Choice.create({
                     type: 'bar' as NonEmptyString,
                     value: 'foo choice' as NonEmptyString,
                  }),
               ],
               type: 'CheckBox' as NonEmptyString,
            }),
         ],
      }),
      Survey.create({
         title: 'Second survey' as NonEmptyString,
         questions: [],
      }),
      Survey.create({
         title: 'Third survey' as NonEmptyString,
         questions: [
            SurveyQuestion.create({
               title: 'q1' as NonEmptyString,
               choices: [
                  Choice.create({
                     type: 'bar' as NonEmptyString,
                     value: 'foo choice' as NonEmptyString,
                  }),
               ],
               type: 'CheckBox' as NonEmptyString,
            }),
            SurveyQuestion.create({
               title: 'q2' as NonEmptyString,
               choices: [
                  Choice.create({
                     type: 'baz' as NonEmptyString,
                     value: 'baz choice' as NonEmptyString,
                  }),
               ],
               type: 'MultiChoice' as NonEmptyString,
            }),
         ],
      })['|>'](Survey.activate),
   ],
   A.map(survey => [survey.id, runEncodeSurvey(survey)] as const),
   Map.make
)

const { decode: decodeSurvey } = strictDecoder(Survey)
export function find(id: UUID) {
   return pipe(
      T.effectTotal(() => O.fromNullable(surveys.get(id))),
      EO.chain(flow(decodeSurvey, EO.fromEffect, T.orDie))
   )
}

export function get(id: UUID) {
   return pipe(
      find(id),
      T.chain(O.fold(() => T.fail(new NotFoundError('Survey', id)), T.succeed))
   )
}

export function all() {
   return pipe(
      T.effectTotal(() => [...surveys.values()] as const),
      T.chain(T.forEach(decodeSurvey)),
      T.orDie
   )
}

export function add(s: Survey) {
   return T.effectTotal(() => {
      surveys = surveys['|>'](Map.insert(s.id, runEncodeSurvey(s)))
   })
}

export function remove(s: Survey) {
   return T.effectTotal(() => {
      surveys = surveys['|>'](Map.remove(s.id))
   })
}
