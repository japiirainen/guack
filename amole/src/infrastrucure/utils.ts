import { getParam } from 'fp-ts-std/Env'
import { getOrElse } from 'fp-ts/Option'
import { pipe } from 'fp-ts/function'

export const getEnvOrElse = (envKey: string, defaultTo: string): string =>
   pipe(
      getParam(envKey)(),
      getOrElse(() => defaultTo)
   )
