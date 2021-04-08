import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import { Request, Response } from '@guack/client/Surveys/DeleteSurvey'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   pipe(SurveyContext.get(_.id), T.chain(SurveyContext.remove), T.asUnit)

export { Request, Response }
