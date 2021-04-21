import fs from 'fs'

import * as Plutus from '@atlas-ts/plutus'
import { JSONSchema, SubSchema } from '@atlas-ts/plutus/JsonSchema'
import { References } from '@atlas-ts/plutus/Schema'
import * as T from '@effect-ts/core/Effect'
import * as L from '@effect-ts/core/Effect/Layer'
import { makeRef } from '@effect-ts/core/Effect/Ref'
import { constVoid, pipe } from '@effect-ts/core/Function'
import * as Ex from '@effect-ts/express'
import * as N from '@effect-ts/node/Runtime'
import { urlencoded, json } from 'body-parser'
import cors from 'cors'

import { routes as surveyRoutes } from './Surveys/routes'
import { AppConfigLive, config } from './infrastrucure/ConfigLayer'
import { logInfo, LoggerLive } from './infrastrucure/LogLayer'

import { makeSchema } from './infrastrucure/routing'

import pkg from 'package.json'

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
   T.tap(rdescs =>
      pipe(
         T.succeedWith(() => {
            console.log(`Running on ${HOST}:${PORT}`)
         }),
         T.zipRight(
            T.gen(function* ($) {
               const ref = yield* $(
                  makeRef<Map<string, JSONSchema | SubSchema>>(new Map())
               )
               const withRef = T.provideService(References)({ ref })
               const paths = yield* $(makeSchema(rdescs.tuple)['|>'](withRef))

               //const test = yield* $(generatePlutus)

               const info = Plutus.info({
                  title: pkg.name,
                  version: pkg.version,
                  pageTitle: pkg.name,
               })
               //            tags: Plutus.tags(Plutus.tag({ name: "Who", description: "Who dunnut" })),
               //        })

               return {
                  openapi: '3.0.0',
                  info: {
                     title: info.title,
                     description: info.description,
                     termsOfService: info.tos,
                     contact: info.contact
                        ? {
                             name: info.contact.name,
                             email: info.contact.email,
                             url: info.contact.url,
                          }
                        : undefined,
                     license: info.license
                        ? {
                             name: info.license.name,
                             url: info.license.url,
                          }
                        : undefined,
                     version: info.version,
                  },
                  //tags,
                  paths,
                  //components: { schemas: refsSchemas, parameters: refsParameters },
                  //test,
               }
            })
         ),
         T.tap(_ =>
            T.effectAsync(cb =>
               fs.writeFile(
                  './schema.json',
                  JSON.stringify(_, undefined, 2),
                  'utf-8',
                  err => (err ? cb(T.fail(err)) : cb(T.succeed(constVoid())))
               )
            )['|>'](T.orDie)
         ),
         T.map(_ => {
            console.log('Available routes: ', JSON.stringify(_, undefined, 2))
         })
      )
   ),
   T.tap(() => logInfo(`${NAME.toUpperCase()} running on ${HOST}:${PORT}`)),
   T.tap(() => T.never)
)

const ExpressLive = Ex.LiveExpress(HOST, +PORT)
const ProgramLive = L.all(LoggerLive, AppConfigLive, ExpressLive)

pipe(program, T.provideSomeLayer(ProgramLive), N.runMain)
