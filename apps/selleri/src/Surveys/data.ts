import * as A from '@effect-ts/core/Collections/Immutable/Array'
import { constant, flow, pipe } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { Lens } from '@effect-ts/monocle'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import * as SurveyClient from '@guack/client'
import * as T from '@guack/types/ext/Effect'
import * as EO from '@guack/types/ext/EffectOption'
import { NonEmptyString } from '@guack/types/shared'
import { useCallback, useEffect, useMemo } from 'react'

import { useServiceContext } from '../infrastructure/context'
import { useFetch, useModify, useQuery } from '../infrastructure/data'

import * as Survey from './Survey'

const fetchLatestSurveys = constant(
   SurveyClient.Surveys.getSurveys['|>'](T.map(r => r.surveys))
)

export function useTasks() {
   const { runWithErrorLog } = useServiceContext()
   const r = useQuery('latestSurveys', fetchLatestSurveys)
   const [, , , exec] = r

   useEffect(() => {
      const cancel = exec()['|>'](runWithErrorLog)
      return () => {
         cancel()
      }
   }, [exec, runWithErrorLog])
   return r
}

const newSurvey = (s: SurveyView) => (newTitle: string) =>
   SurveyClient.Surveys.createSurveyE({
      title: newTitle,
      ...(s === 'closed' ? { isClosed: true } : { isClosed: false }),
   })

export const SurveyView = ['open', 'closed'] as const
export type SurveyView = typeof SurveyView[number]

export function useNewSurvey(v: SurveyView) {
   return useFetch(newSurvey(v))
}
export function useFindSurvey() {
   return useFetch(SurveyClient.Surveys.findSurvey)
}

const deleteSurvey = (id: UUID) => SurveyClient.Surveys.deleteSurvey({ id })

export function useDeleteSurvey() {
   return useFetch(deleteSurvey)
}

export function useUpdateSurvey() {
   return useFetch(SurveyClient.Surveys.updateSurvey)
}

export function useUpdateSurvey2(id: string) {
   // lets use the refetch for now, but in future make a mutation queue e.g via semaphore
   // or limit to alwaus just 1
   return useQuery(`update-survey-$${id}`, SurveyClient.Surveys.updateSurvey)
}

export function useModifySurveys() {
   return useModify<A.Array<Survey.Survey>>('latestSurveys')
}

export function useGetSurvey() {
   const modifySurveys = useModifySurveys()
   const [findResult, findSurvey] = useFindSurvey()
   return [
      findResult,
      useCallback(
         (id: UUID) =>
            pipe(
               findSurvey(id),
               EO.tap(s =>
                  T.effectTotal(() =>
                     modifySurveys(surveys =>
                        pipe(
                           A.findIndex_(surveys, x => x.id === s.id),
                           O.chain(i => A.modifyAt_(surveys, i, constant(s))),
                           O.getOrElse(() => A.cons_(surveys, s))
                        )
                     )
                  )
               )
            ),
         [findSurvey, modifySurveys]
      ),
   ] as const
}

export function useSurveyCommands(id: UUID) {
   const modifySurveys = useModifySurveys()

   const [updateResult, , updateSurvey] = useUpdateSurvey2(id)

   const [findResult, getSurvey] = useGetSurvey()

   const funcs = useMemo(() => {
      const refreshSurvey = (s: { id: UUID }) => getSurvey(s.id)

      const updateAndRefreshSurvey = (r: SurveyClient.Surveys.UpdateSurvey.Request) =>
         pipe(updateSurvey(r), T.zipRight(refreshSurvey(r)))

      function updateSurveyQuestionIndex(s: Survey.Survey) {
         return (sq: Survey.SurveyQuestion) => (newIndex: number) => {
            const updatedSurvey = s['|>'](Survey.Survey.updateQuestionIndex(sq, newIndex))
            modifySurveys(A.map(_ => (_.id === s.id ? updatedSurvey : _)))
            return updateAndRefreshSurvey(updatedSurvey)
         }
      }
      function updateSurveyQuestionTitle(s: Survey.Survey) {
         return (sq: Survey.SurveyQuestion) =>
            flow(
               NonEmptyString.parse,
               T.map(title => s['|>'](Survey.Survey.updateQuestion(sq, title))),
               T.chain(updateAndRefreshSurvey)
            )
      }

      function setTitle(s: Survey.Survey) {
         return flow(
            NonEmptyString.parse,
            T.map(v => s['|>'](Survey.Survey.lens['|>'](Lens.prop('title')).set(v))),
            T.chain(updateAndRefreshSurvey)
         )
      }

      return {
         updateSurveyQuestionIndex,
         updateSurveyQuestionTitle,
         setTitle,
      }
   }, [getSurvey, modifySurveys, updateSurvey])

   return {
      ...funcs,
      findResult,
      updateResult,
   }
}

export type SurveyCommand = ReturnType<typeof useSurveyCommands>
