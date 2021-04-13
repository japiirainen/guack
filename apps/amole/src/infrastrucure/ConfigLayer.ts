import * as T from '@effect-ts/core/Effect'
import * as L from '@effect-ts/core/Effect/Layer'
import * as M from '@effect-ts/core/Effect/Managed'
import { pipe } from '@effect-ts/core/Function'
import { tag } from '@effect-ts/system/Has'
import dotenv from 'dotenv'

dotenv.config()
import { getEnvOrElse } from './utils'

export interface AppConfig {
   appPort: T.UIO<string>
   appName: T.UIO<string>
   appHost: T.UIO<string>
   pgURL: T.UIO<string>
   close: (msg: string) => T.UIO<void>
}

export const AppConfig = tag<AppConfig>()

export const AppConfigLive = pipe(
   T.effectTotal(
      (): AppConfig => ({
         appPort: T.effectTotal(() => getEnvOrElse('PORT', '4200')),
         appName: T.effectTotal(() => getEnvOrElse('PORT', 'amole')),
         appHost: T.effectTotal(() => getEnvOrElse('PORT', '127.0.0.1')),
         pgURL: T.effectTotal(() =>
            getEnvOrElse(
               'PORT',
               'postgres://guackamole:salaisuus@localhost:5432/guack-pg'
            )
         ),
         close: msg => T.effectTotal(() => console.log(msg)),
      })
   ),
   M.make(_ => _.close('closing app config')),
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
