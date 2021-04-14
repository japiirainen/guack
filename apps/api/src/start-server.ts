import * as T from '@effect-ts/core/Effect'
import * as L from '@effect-ts/core/Effect/Layer'
import { pipe } from '@effect-ts/core/Function'
import * as Ex from '@effect-ts/express'
import * as N from '@effect-ts/node/Runtime'
import { urlencoded, json } from 'body-parser'
import cors from 'cors'
import morgan from 'morgan'

import { routes as surveyRoutes } from './Surveys/routes'
import { AppConfigLive, config } from './infrastrucure/ConfigLayer'
import { logInfo, LoggerLive } from './infrastrucure/LogLayer'

const {
   application: { HOST, NAME, PORT },
} = config

const program = pipe(
   T.tuple(
      Ex.use(Ex.classic(cors())),
      Ex.use(Ex.classic(urlencoded({ extended: false }))),
      Ex.use(Ex.classic(json())),
      Ex.use(Ex.classic(morgan('dev')))
   ),
   T.zipRight(surveyRoutes),
   T.tap(() => logInfo(`${NAME.toUpperCase()} running on ${HOST}:${PORT}`)),
   T.tap(() => T.never)
)

const ExpressLive = Ex.LiveExpress(HOST, +PORT)
const ProgramLive = L.all(LoggerLive, AppConfigLive, ExpressLive)

pipe(program, T.provideSomeLayer(ProgramLive), N.runMain)
