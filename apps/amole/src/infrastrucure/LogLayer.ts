import * as T from '@effect-ts/core/Effect'
import * as L from '@effect-ts/core/Effect/Layer'
import * as M from '@effect-ts/core/Effect/Managed'
import { pipe } from '@effect-ts/core/Function'
import { tag } from '@effect-ts/system/Has'
import pino from 'pino'

export interface Logger {
   readonly logInfo: (k: string) => T.UIO<void>
   readonly logError: (k: string) => T.UIO<void>
   readonly logDebug: (k: string) => T.UIO<void>
   readonly flush: T.UIO<void>
}
export const Logger = tag<Logger>()

const prodLog = pino({
   prettyPrint: {
      levelFirst: true,
   },
   prettifier: require('pino-pretty'),
})

export const LoggerLive = pipe(
   T.effectTotal(
      (): Logger => ({
         logInfo: msg => T.effectTotal(() => prodLog.info(msg)),
         logError: msg => T.effectTotal(() => prodLog.error(msg)),
         logDebug: msg => T.effectTotal(() => prodLog.debug(msg)),
         flush: T.effectTotal(() => prodLog.flush()),
      })
   ),
   M.make(_ => _.flush),
   L.fromManaged(Logger)
)

export const { logDebug, logError, logInfo } = T.deriveLifted(Logger)(
   ['logInfo', 'logDebug', 'logError'],
   [],
   []
)
