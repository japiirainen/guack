import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import { Request, Response } from '@guack/client/Surveys/GetSurveys'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   pipe(
      SurveyContext.all(),
      T.map(ss => ({ surveys: ss }))
   )

export { Request, Response }
