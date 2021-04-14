import { encode as htmlEncode } from 'html-entities'
import _ from 'lodash'
import { applyMiddleware, router } from 'typera-express'
import { wrapNative } from 'typera-express/middleware'
import { body, headers } from 'typera-express/parser'
import { badRequest, internalServerError, ok } from 'typera-common/response'
import * as t from 'io-ts'
import { NonEmptyString } from 'io-ts-types'
import bodyParser from 'body-parser'

const route = applyMiddleware(wrapNative(bodyParser.json()))
const apiTokenHeader = headers(t.partial({ API_TOKEN: t.string }))
