import * as M from '@effect-ts/core/Effect/Managed'
import * as PG from 'pg'
import * as T from '@effect-ts/core/Effect'
import { Has, tag } from '@effect-ts/core/Has'
import * as L from '@effect-ts/core/Effect/Layer'
import { _A } from '@effect-ts/core/Utils'
import { pgURL } from './ConfigLayer'
import { literal, pipe } from '@effect-ts/core/Function'

export const makePgPoolConfig = T.gen(function* (_) {
   const connectionString = yield* _(pgURL)

   return {
      _tag: literal('PgPoolConfig'),
      config: <PG.PoolConfig>{
         connectionString,
      },
   }
})

export interface PgPoolConfig extends _A<typeof makePgPoolConfig> {}
export const PgPoolConfig = tag<PgPoolConfig>()

export const LivePgPoolConfig = L.fromEffect(PgPoolConfig)(makePgPoolConfig)

export const makePgPool = pipe(
   T.accessService(PgPoolConfig)(_ => _.config),
   T.chain(config =>
      T.succeedWith(() => ({
         _tag: literal('PgPool'),
         pool: new PG.Pool(config),
      }))
   ),
   M.makeExit(({ pool }) => T.promise(() => pool.end()))
)

export interface PgPool extends _A<typeof makePgPool> {}
export const PgPool = tag<PgPool>()

export const LivePgPool = L.fromManaged(PgPool)(makePgPool)
export const LivePg = LivePgPoolConfig['>+>'](LivePgPool)

export const makePgConnection = M.gen(function* (_) {
   const { pool } = yield* _(PgPool)

   const connection = yield* _(
      pipe(
         T.promise(() => pool.connect()),
         M.makeExit(client => T.succeedWith(() => client.release()))
      )
   )

   return {
      _tag: literal('PgConnection'),
      connection,
   }
})

export interface PgConnection extends _A<typeof makePgConnection> {}
export const PgConnection = tag<PgConnection>()

export function provideLivePgConnection<R, E, A>(
   self: T.Effect<R & Has<PgConnection>, E, A>
) {
   return pipe(
      makePgConnection,
      M.use(connection => T.provideService(PgConnection)(connection)(self))
   )
}
