import { Fiber, semaphore } from '@effect-ts/core'
import * as T from '@effect-ts/core/Effect'
import { Cause } from '@effect-ts/core/Effect/Cause'
import * as Ex from '@effect-ts/core/Effect/Exit'
import { Exit } from '@effect-ts/core/Effect/Exit'
import { pipe } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import * as Arr from '@guack/types/ext/Array'
import { datumEither } from '@nll/datum'
import { DatumEither } from '@nll/datum/DatumEither'
import { useState, useCallback, useEffect, useMemo } from 'react'

import { Fetcher, useFetchContext } from './context'

export function useFetch<R, E, A, Args extends readonly unknown[]>(
   fetchFnc: (...args: Args) => T.Effect<R, E, A>
) {
   const [result, setResult] = useState<DatumEither<E, A>>(datumEither.constInitial())
   const exec = useCallback(
      function (...args: Args) {
         return pipe(
            T.effectTotal(() => setResult(datumEither.constInitial())),
            T.zipRight(fetchFnc(...args)),
            T.tap(a =>
               T.effectTotal(() => {
                  setResult(datumEither.success(a))
               })
            ),
            T.catchAll(err => {
               setResult(datumEither.failure(err))
               return T.fail(err)
            })
         )
      },
      [fetchFnc]
   )
   return [result, exec] as const
}

export function useLimitToOne<R, E, A, Args extends readonly unknown[]>(
   exec: (...args: Args) => T.Effect<R, E, A>
) {
   // eslint-disable-next-line @typescript-eslint/ban-types
   const [cancel, setCancel] = useState<() => {}>(() => () => void 0)
   return useCallback(
      (...args: Args) => limitToOne(cancel, cncl => setCancel(() => cncl))(exec(...args)),
      [cancel, exec]
   )
}

function limitToOne(cancel: () => void, setCancel: (cnl: () => void) => void) {
   return <R, E, A>(self: T.Effect<R, E, A>) =>
      pipe(
         T.effectTotal(() => {
            console.log('cancel', cancel)
            cancel()
         }),
         T.zipRight(
            pipe(
               self,
               T.fork,
               // NOTE; actually the cancellation means that running to Promise will also not resolve on the success channel.
               // thus additional callbacks will fail.
               T.tap(f => T.effectTotal(() => setCancel(() => T.run(Fiber.interrupt(f)))))
            )
         ),
         T.chain(Fiber.join)
      )
}

export function useModify<A>(name: string) {
   const ctx = useFetchContext()
   const modify = useCallback()
}
