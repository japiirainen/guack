import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import { Request, Response } from '@guack/client/Surveys/GetSurveys'

import * as SurveyContext from './SurveysContext'
import { Survey } from '@guack/types'

export const handle = (_: Request) =>
   pipe(
      SurveyContext.all,
      T.map(ss => ({ surveys: ss }))
   )

export const handle_ = (_: Response) =>
   T.gen(function* ($) {
      const surveys = yield* $(SurveyContext.allOrdered)

      return {
         surveys: surveys as readonly Survey[],
      }
   })

export { Request, Response }
