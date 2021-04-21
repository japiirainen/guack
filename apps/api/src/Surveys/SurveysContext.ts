import * as A from '@effect-ts/core/Collections/Immutable/Array'
import * as Map from '@effect-ts/core/Collections/Immutable/Map'
import * as T from '@effect-ts/core/Effect'
import { flow, pipe } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import * as Sy from '@effect-ts/core/Sync'
import * as Ref from '@effect-ts/core/Effect/Ref'
import * as Chunk from '@effect-ts/core/Collections/Immutable/Chunk'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import { encode } from '@effect-ts/morphic/Encoder'
import { strict } from '@effect-ts/morphic/Strict'
import { strictDecoder } from '@effect-ts/morphic/StrictDecoder'
import { Survey, SurveyE, SurveyQuestion, Choice } from '@guack/types'
import * as EO from '@guack/types/ext/EffectOption'
import { NonEmptyString } from '@guack/types/shared'

import { NotFoundError } from '../infrastrucure/errors'
import { Ord } from '@effect-ts/core/Ord'

const encodeSurvey = flow(strict(Survey).shrink, Sy.chain(encode(Survey)))
const runEncodeSurvey = flow(encodeSurvey, Sy.run)

const surveysRef = Ref.unsafeMakeRef<Map.Map<UUID, SurveyE>>(
   pipe(
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
)

const { decode: decodeSurvey } = strictDecoder(Survey)
export function find(id: UUID) {
   return pipe(
      surveysRef.get['|>'](T.map(surveys => O.fromNullable(surveys.get(id)))),
      EO.chain(flow(decodeSurvey, EO.fromEffect, T.orDie))
   )
}

export function get(id: UUID) {
   return pipe(
      find(id),
      T.chain(O.fold(() => T.fail(new NotFoundError('Survey', id)), T.succeed))
   )
}

export const all = pipe(
   surveysRef.get,
   T.chain(surveys => T.forEach_(surveys.values(), decodeSurvey)),
   T.orDie
)

export function add(s: Survey) {
   return pipe(
      T.structPar({ encS: encodeSurvey(s), surveys: surveysRef.get }),
      T.chain(({ encS, surveys }) =>
         surveysRef.set(surveys['|>'](Map.insert(s.id, encS)))
      )
   )
}

export function remove(s: Survey) {
   return surveysRef.get['|>'](
      T.chain(surveys => surveysRef.set(surveys['|>'](Map.remove(s.id))))
   )
}
const orderRef = Ref.unsafeMakeRef<A.Array<UUID>>([])

export const getOrder = orderRef.get
export const setOrder = orderRef.set

export const allOrdered = pipe(
   T.structPar({ surveys: all, order: getOrder }),
   T.map(({ order, surveys }) => orderSurveys(surveys['|>'](Chunk.toArray), order))
)

function orderSurveys(a: A.Array<Survey>, order: A.Array<UUID>) {
   return A.reverse(a)['|>'](A.sort(makeOrd(order)))
}

function makeOrd(sortingArr: A.Array<UUID>): Ord<Survey> {
   return {
      compare: (a, b) => {
         const diff = sortingArr.indexOf(a.id) - sortingArr.indexOf(b.id)
         return diff > 1 ? 1 : diff < 0 ? -1 : 0
      },
   }
}
