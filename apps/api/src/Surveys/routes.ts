import * as T from '@effect-ts/core/Effect'
import * as Ex from '@effect-ts/express'

import { makeRequestHandler } from '../infrastrucure/requestHandler'

import * as CreateSurvey from './CreateSurvey'
import * as DeleteSurvey from './DeleteSurvey'
import * as GetSurvey from './GetSurvey'
import * as GetSurveys from './GetSurveys'
import * as UpdateSurvey from './UpdateSurvey'

export const routes = T.tuple(
   Ex.get('/surveys/:id', makeRequestHandler(GetSurvey)),
   Ex.get('/surveys', makeRequestHandler(GetSurveys)),
   Ex.patch('/surveys/:id', makeRequestHandler(UpdateSurvey)),
   Ex.delete('/surveys/:id', makeRequestHandler(DeleteSurvey)),
   Ex.post('/surveys', makeRequestHandler(CreateSurvey))
)
