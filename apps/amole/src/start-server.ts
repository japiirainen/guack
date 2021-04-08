import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import * as Ex from '@effect-ts/express'
import * as N from '@effect-ts/node/Runtime'
import { urlencoded, json } from 'body-parser'
import cors from 'cors'

import { config } from './infrastrucure/config'
import { info } from './infrastrucure/log'
import { routes as surveyRoutes } from './Surveys/routes'

const {
   application: { HOST, NAME, PORT },
} = config

const program = pipe(
   T.tuple(
      Ex.use(Ex.classic(cors())),
      Ex.use(Ex.classic(urlencoded({ extended: false }))),
      Ex.use(Ex.classic(json()))
   ),
   T.zipRight(surveyRoutes),
   T.tap(() => T.effectTotal(() => info(`${NAME} running on ${HOST}:${PORT}`))),
   T.tap(() => T.never)
)

pipe(program, T.provideSomeLayer(Ex.LiveExpress(HOST, +PORT)), N.runMain)
