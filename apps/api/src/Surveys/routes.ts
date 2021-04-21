import * as T from '@effect-ts/core/Effect'

import * as R from '../infrastrucure/routing'

import * as CreateSurvey from './CreateSurvey'
import * as DeleteSurvey from './DeleteSurvey'
import * as GetSurvey from './GetSurvey'
import * as GetSurveys from './GetSurveys'
import * as UpdateSurvey from './UpdateSurvey'
import * as SetSurveyOrder from './SetSurveysOrder'

export const routes = T.tuple(
   R.get('/surveys/:id', GetSurvey),
   R.get('/surveys', GetSurveys),
   R.patch('/surveys/:id', UpdateSurvey),
   R.delete('/surveys/:id', DeleteSurvey),
   R.post('/surveys', CreateSurvey),
   R.post('/surveys-order', SetSurveyOrder)
)
