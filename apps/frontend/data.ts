import { ParsedUrlQuery } from 'querystring'

import { Fiber, Semaphore } from '@effect-ts/core'
import * as T from '@effect-ts/core/Effect'
import { Cause } from '@effect-ts/core/Effect/Cause'
import * as Ex from '@effect-ts/core/Effect/Exit'
import { Exit } from '@effect-ts/core/Effect/Exit'
import * as E from '@effect-ts/core/Either'
import { pipe, flow } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import * as Sy from '@effect-ts/core/Sync'
import { AType, M } from '@effect-ts/morphic'
import { decode } from '@effect-ts/morphic/Decoder'
import * as Arr from '@guack/types/ext/Array'
import { datumEither } from '@nll/datum'
import { DatumEither } from '@nll/datum/DatumEither'
import { useRouter } from 'next/router'
import React, { useState } from 'react'

import { Fetcher, useFetchContext } from './context'
import { typedKeysOf } from './utils'

export type WithLoading<Fnc> = Fnc & {
   loading: boolean
}

export function useFetch<R, E, A, Args extends readonly unknown[]>(
   fetchFnc: (...args: Args) => T.Effect<R, E, A>
) {
   const [result, setResult] = useState<DatumEither<E, A>>(datumEither.constInitial())
   const exec = useCallback(
      function (...args: Args) {
         return pipe(
            T.succeedWith(() => setResult(datumEither.constInitial())),
            T.zipRight(fetchFnc(...args)),
            T.tap(a =>
               T.succeedWith(() => {
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
         T.succeedWith(() => {
            console.log('cancel', cancel)
            cancel()
         }),
         T.zipRight(
            pipe(
               self,
               T.fork,
               // NOTE; actually the cancellation means that running to Promise will also not resolve on the success channel.
               // thus additional callbacks will fail.
               T.tap(f => T.succeedWith(() => setCancel(() => T.run(Fiber.interrupt(f)))))
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
            T.succeedWith(() => {
               fetcher.modify(r =>
                  datumEither.isInitial(r)
                     ? datumEither.constPending()
                     : datumEither.toRefresh(r)
               )
            }),
            T.zipRight(fetcher.fetch(...args)),
            T.chain(a =>
               T.succeedWith(() => {
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
            T.succeedWith(() => fetcher.fiber),
            T.chain(f =>
               f
                  ? T.succeedWith(() => {
                       console.log('Joining existing fiber', f.id)
                       return f
                    })
                  : pipe(
                       ff(...args),
                       T.fork,
                       T.tap(f =>
                          T.succeedWith(() => {
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
               const setFiber = T.succeedWith(() => {
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

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function shallowEqual(objA: any, objB: any) {
   if (Object.is(objA, objB)) {
      return true
   }

   if (
      typeof objA !== 'object' ||
      objA === null ||
      typeof objB !== 'object' ||
      objB === null
   ) {
      return false
   }

   const keysA = Object.keys(objA)
   const keysB = Object.keys(objB)

   if (keysA.length !== keysB.length) {
      return false
   }

   // Test for A's keys different from B.
   for (let i = 0; i < keysA.length; i++) {
      const propA = objA[keysA[i]]
      const propB = objB[keysA[i]]
      if (!Object.hasOwnProperty.call(objB, keysA[i])) {
         return false
      }
      if (!Object.is(propA, propB)) {
         if (
            typeof propA !== 'object' ||
            typeof propB !== 'object' ||
            propA === null ||
            propB === null ||
            !propA._tag ||
            propA._tag !== propB._tag
         ) {
            return false
         }

         if (
            O.isNone(propA) ||
            (O.isSome(propA) && Object.is(propA.value, propB.value)) ||
            (E.isLeft(propA) && Object.is(propA.left, propB.left)) ||
            (E.isRight(propA) && Object.is(propA.right, propB.right))
         ) {
            return true
         }
         return false
      }
   }

   return true
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function memo<T extends React.ComponentType<any>>(f: T) {
   return React.memo(f, shallowEqual)
}

export function useEffect(effect: React.EffectCallback, deps: React.DependencyList) {
   // eslint-disable-next-line react-hooks/exhaustive-deps
   return React.useEffect(effect, mapDeps(deps))
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
export function useCallback<T extends (...args: any[]) => any>(
   callback: T,
   deps: React.DependencyList
) {
   // eslint-disable-next-line react-hooks/exhaustive-deps
   return React.useCallback(callback, mapDeps(deps))
}

export function useMemo<T>(factory: () => T, deps: React.DependencyList) {
   // eslint-disable-next-line react-hooks/exhaustive-deps
   return React.useMemo(factory, mapDeps(deps))
}

function mapDeps(deps: React.DependencyList) {
   return deps.map(convertDep)
}

// eslint-disable-next-line @typescript-eslint/no-explicit-any
function convertDep(x: any) {
   return typeof x !== 'object' || x === null
      ? x
      : O.isSome(x) || O.isNone(x)
      ? O.toNullable(x)
      : E.isLeft(x)
      ? x.left
      : E.isRight(x)
      ? x.right
      : x
}

export function getQueryParam(search: ParsedUrlQuery, param: string) {
   const v = search[param]
   if (Array.isArray(v)) {
      return v[0]
   }
   return v ?? null
}

export const parseOption = <E, A>(t: M<{}, E, A>) => {
   const dec = decode(t)
   return (_: E) => dec(_)['|>'](Sy.runEither)['|>'](O.fromEither)
}
export const getQueryParamO = flow(getQueryParam, O.fromNullable)

export const useRouteParam = <A>(t: M<{}, string, A>, key: string) => {
   const r = useRouter()
   return getQueryParamO(r.query, key)['|>'](O.chain(parseOption(t)))
}

export function useReportLoading(name: string) {
   return useEffect(() => {
      console.log('$$$ loaded', name)

      return () => console.log('$$$ unloaded', name)
   }, [name])
}

export function withLoading<Fnc>(fnc: Fnc, loading: boolean): WithLoading<Fnc> {
   return Object.assign(fnc, { loading })
}

export const useRouteParams = <NER extends Record<string, M<{}, string, any>>>(
   t: NER // enforce non empty
): {
   [K in keyof NER]: O.Option<AType<NER[K]>>
} => {
   const r = useRouter()
   return typedKeysOf(t).reduce(
      (prev, cur) => {
         prev[cur] = getQueryParamO(r.query, cur as string)['|>'](
            O.chain(parseOption(t[cur]))
         )
         return prev
      },
      {} as {
         [K in keyof NER]: AType<NER[K]>
      }
   )
}
