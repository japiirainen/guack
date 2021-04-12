import { Fiber, Semaphore } from '@effect-ts/core'
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
   const modify = useCallback(
      (mod: (a: A) => A) => {
         const f = ctx.fetchers[name] as {
            result: DatumEither<unknown, A>
            latestSuccess: DatumEither<unknown, A>
            update: (
               result: DatumEither<unknown, A>,
               latestSuccess: DatumEither<unknown, A>
            ) => void
         }
         const result = f.result['|>'](datumEither.map(mod))
         const latestSuccess = f.latestSuccess['|>'](datumEither.map(mod))
         f.update(result, latestSuccess)
      },
      [ctx.fetchers, name]
   )

   return modify
}

/**
 * Able to use the query over and over in multiple components, but still sharing the same state.
 * TODO should only share the result when variables are the same
 * TODO use ref
 * */
export function useQuery<R, E, A, Args extends ReadonlyArray<unknown>>(
   name: string,
   fetchFunction: (...args: Args) => T.Effect<R, E, A>
) {
   const ctx = useFetchContext()
   // const { runWithErrorLog } = useServiceContext()
   type FetchFnc = typeof fetchFunction
   type F = Fetcher<E, E, A, FetchFnc>

   if (ctx.fetchers[name]) {
      if (fetchFunction !== ctx.fetchers[name].fetch) {
         console.warn(`Fetch function for ${name} appears to be unstable`)
         ctx.fetchers[name].fetch = fetchFunction
      }
   } else {
      const fetcher: F = {
         cancel: () => void 0,
         fiber: null,
         fetch: fetchFunction,
         result: datumEither.constInitial(),
         latestSuccess: datumEither.constInitial(),
         listeners: [],
         modify: mod => fetcher.update(mod(fetcher.result)),
         update: (result, latestSuccess) => {
            fetcher.result = result
            if (latestSuccess) {
               fetcher.latestSuccess = latestSuccess
            } else {
               latestSuccess = fetcher.latestSuccess = datumEither.isSuccess(result)
                  ? result
                  : fetcher.latestSuccess
            }
            // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
            fetcher.listeners.forEach(f => f(result, latestSuccess!))
         },
         sync: Semaphore.unsafeMakeSemaphore(1),
      }
      ctx.fetchers[name] = fetcher
   }

   const fetcher = useMemo(() => ctx.fetchers[name] as F, [ctx.fetchers, name])

   const modify = useModify<A>(name)

   // TODO just store inside the context
   const ff = useCallback(
      function (...args: Args) {
         return pipe(
            T.effectTotal(() => {
               fetcher.modify(r =>
                  datumEither.isInitial(r)
                     ? datumEither.constPending()
                     : datumEither.toRefresh(r)
               )
            }),
            T.zipRight(fetcher.fetch(...args)),
            T.chain(a =>
               T.effectTotal(() => {
                  fetcher.update(datumEither.success(a))
                  return a
               })
            ),
            T.catchAll(err => {
               fetcher.update(datumEither.failure(err))
               return T.fail(err)
            }),
            T.result,
            T.chain(
               Ex.foldM(
                  cause => {
                     console.warn(`exiting on cause`, cause)
                     return T.halt(cause)
                  },
                  v => {
                     return T.succeed(v)
                  }
               )
            )
         )
      },
      [fetcher]
   )

   // joins the existing fiber when available, even when old,
   const exec = useCallback(
      function (...args: Args) {
         return pipe(
            T.effectTotal(() => fetcher.fiber),
            T.chain(f =>
               f
                  ? T.effectTotal(() => {
                       console.log('Joining existing fiber', f.id)
                       return f
                    })
                  : pipe(
                       ff(...args),
                       T.fork,
                       T.tap(f =>
                          T.effectTotal(() => {
                             console.log('setting fiber', f.id)
                             fetcher.fiber = f
                          })
                       )
                    )
            ),
            Semaphore.withPermit(fetcher.sync),
            T.chain(Fiber.join)
         )
      },
      [ff, fetcher]
   )

   // kills existing fiber when available and refetches
   const refetch = useCallback(
      (...args: Args) =>
         pipe(
            ff(...args),
            T.fork,
            T.tap(f => {
               const runFiber = fetcher.fiber
               if (runFiber) {
                  console.log('interrupting fiber', runFiber.id)
               }
               const setFiber = T.effectTotal(() => {
                  console.log('setting fiber', f.id)
                  fetcher.fiber = f
               })
               return runFiber
                  ? Fiber.interrupt(runFiber)['|>'](T.chain(() => setFiber))
                  : setFiber
            }),
            Semaphore.withPermit(fetcher.sync),
            T.chain(Fiber.join)
         ),

      [ff, fetcher]
   )
   const [{ latestSuccess, result }, setResult] = useState<
      Pick<F, 'result' | 'latestSuccess'>
   >(() => ({
      result: fetcher.result,
      latestSuccess: fetcher.result,
   }))

   useEffect(() => {
      setResult({ result: fetcher.result, latestSuccess: fetcher.latestSuccess })
      const handler = (result: F['result'], latestSuccess: F['latestSuccess']) => {
         setResult({ result, latestSuccess })
      }
      fetcher.listeners = Arr.snoc_(fetcher.listeners, handler)
      return () => {
         Arr.findIndex_(fetcher.listeners, x => x === handler)
            ['|>'](O.chain(idx => Arr.deleteAt_(fetcher.listeners, idx)))
            ['|>'](
               O.fold(
                  // eslint-disable-next-line @typescript-eslint/no-empty-function
                  () => {},
                  l => (fetcher.listeners = l)
               )
            )
      }
   }, [fetcher])

   return [result, latestSuccess, refetch, exec, modify] as const
}

export type PromiseExit<E = unknown, A = unknown> = Promise<Exit<E, A>>

export function onFail<E, T>(cb: (a: Cause<E>) => T) {
   return Ex.fold(cb, () => void 0)
}

export function onSuccess<A, T>(cb: (a: A) => T) {
   return Ex.fold(() => void 0, cb)
}
