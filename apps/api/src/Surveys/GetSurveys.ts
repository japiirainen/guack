import * as T from '@effect-ts/core/Effect'
import { Request, Response } from '@guack/client/Surveys/GetSurveys'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   T.gen(function* ($) {
      const items = yield* $(SurveyContext.allOrdered)

      return {
         items,
      }
   })

export { Request, Response }
