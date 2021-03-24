import * as C from 'fp-ts/lib/Console'
import { IO } from 'fp-ts/lib/IO'
import * as L from 'logging-ts/lib/IO'
import * as D from 'fp-ts/lib/Date'
import { chain } from 'fp-ts/lib/IO'
import { pipe } from 'fp-ts/pipeable'

type Level = 'Debug' | 'Info' | 'Warning' | 'Error'

interface Entry {
   message: string
   time: Date
   level: Level
}

const showEntry = (entry: Entry): string =>
   `[${entry.level}] ${entry.time.toLocaleString()} ${entry.message}`

const getLoggerEntry = (prefix: string): L.LoggerIO<Entry> => entry =>
   C.log(`${prefix}: ${showEntry(entry)}`)

const debugLogger = L.filter(getLoggerEntry('debug.log'), e => e.level === 'Debug')
const productionLogger = L.filter(getLoggerEntry('production.log'), e => e.level !== 'Debug')

const logger = L.getMonoid<Entry>().concat(debugLogger, productionLogger)

export const debug = (message: string): IO<void> =>
   pipe(
      D.create,
      chain(time => logger({ message, time, level: 'Debug' }))
   )

export const info = (message: string): IO<void> =>
   pipe(
      D.create,
      chain(time => logger({ message, time, level: 'Info' }))
   )

export const warning = (message: string): IO<void> =>
   pipe(
      D.create,
      chain(time => logger({ message, time, level: 'Warning' }))
   )

export const error = (message: string): IO<void> =>
   pipe(
      D.create,
      chain(time => logger({ message, time, level: 'Error' }))
   )
