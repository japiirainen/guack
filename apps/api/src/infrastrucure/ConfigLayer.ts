import * as T from '@effect-ts/core/Effect'
import * as L from '@effect-ts/core/Effect/Layer'
import * as M from '@effect-ts/core/Effect/Managed'
import { pipe } from '@effect-ts/core/Function'
import { tag } from '@effect-ts/system/Has'
import dotenv from 'dotenv'

dotenv.config()
import { logInfo } from './LogLayer'
import { getEnvOrElse } from './utils'

export class MissingEnvVar {
   readonly _tag = 'MissingEnvVar'
   constructor(readonly key: string) {}
}

export function readEnv(key: string): T.UIO<string> {
   return T.suspend(() => {
      const value = process.env[key]

      if (!value) {
         return T.die(new MissingEnvVar(key))
      }
      return T.succeed(value)
   })
}

export interface AppConfig {
   appPort: T.UIO<string>
   appName: T.UIO<string>
   appHost: T.UIO<string>
   pgURL: T.UIO<string>
   close: (msg: string) => T.UIO<void>
}

export const AppConfig = tag<AppConfig>()

export const AppConfigLive = pipe(
   T.succeedWith(
      (): AppConfig => ({
         appPort: readEnv('PORT'),
         appName: readEnv('NAME'),
         appHost: readEnv('HOST'),
         pgURL: readEnv('DATABASE_URL'),
         close: msg => T.succeedWith(() => logInfo(msg)),
      })
   ),
   M.make(_ => _.close('closing managed')),
   L.fromManaged(AppConfig)
)

export const { appHost, appName, appPort, pgURL } = T.deriveLifted(AppConfig)(
   [],
   ['appHost', 'appName', 'appPort', 'pgURL'],
   []
)

export const config = {
   application: {
      PORT: getEnvOrElse('PORT', '4200'),
      NAME: getEnvOrElse('NAME', 'amole'),
      HOST: getEnvOrElse('HOST', '127.0.0.1'),
   },
   DATABASE_URL: getEnvOrElse(
      'DATABASE_URL',
      'postgres://guackamole:salaisuus@localhost:5432/guack-pg'
   ),
}
