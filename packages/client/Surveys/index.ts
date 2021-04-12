import { pipe } from '@effect-ts/core'
import * as T from '@effect-ts/core/Effect'
import { flow } from '@effect-ts/core/Function'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import { decode } from '@effect-ts/morphic/Decoder'

import { fetchApi, fetchApi3, mapResponseError } from '../fetch'

import * as CreateSurvey from './CreateSurvey'
import * as DeleteSurvey from './DeleteSurvey'
import * as GetSurvey from './GetSurvey'
import * as GetSurveys from './GetSurveys'
import * as UpdateSurvey from './UpdateSurvey'

const decodeGetSurveysResponse = flow(decode(GetSurveys.Response), mapResponseError)
export const getSurveys = pipe(fetchApi('/surveys'), T.chain(decodeGetSurveysResponse))

const decodeGetSurveyResponse = flow(decode(GetSurvey.Response), mapResponseError)
export const findSurvey = (id: UUID) =>
   pipe(fetchApi(`/surveys/${id}`), T.chain(decodeGetSurveyResponse))

export const createSurvey = fetchApi3(CreateSurvey)('/surveys')

const decodeCreateSurveyRequest = decode(CreateSurvey.Request)
export const createSurveyE = flow(decodeCreateSurveyRequest, T.chain(createSurvey))

const update = fetchApi3(UpdateSurvey, 'PATCH')
export function updateSurvey(req: UpdateSurvey.Request) {
   return update(`/surveys/${req.id}`)(req)
}

const del = fetchApi3(DeleteSurvey, 'DELETE')
export function deleteSurvey(req: DeleteSurvey.Request) {
   return del(`/surveys/${req.id}`)(req)
}

export * as CreateSurvey from './CreateSurvey'
export * as DeleteSurvey from './DeleteSurvey'
export * as GetSurvey from './GetSurvey'
export * as GetSurveys from './GetSurveys'
export * as UpdateSurvey from './UpdateSurvey'
