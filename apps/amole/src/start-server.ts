import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import * as Ex from '@effect-ts/express'
import { urlencoded, json } from 'body-parser'
import cors from 'cors'

import { config } from './infrastrucure/config'
import { error, info } from './infrastrucure/log'
//import { routes as surveyRoutes } from './Surveys/routes'

const {
   application: { PORT, HOST },
} = config

const program = pipe(
   T.tuple(
      Ex.use(Ex.classic(cors())),
      Ex.use(Ex.classic(urlencoded({ extended: false }))),
      Ex.use(Ex.classic(json()))
   )
)

pipe(program, T.provideSomeLayer(Ex.LiveExpress(HOST, +PORT)), T.runPromiseExit)
   .then(info(`listening on: ${PORT}`))
   .catch(e => error(e.message)())
