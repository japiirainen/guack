import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { Request, Response } from '@guack/client/Surveys/GetSurvey'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   pipe(
      SurveyContext.get(_.id),
      T.map(s => O.fromNullable(s))
   )

export { Request, Response }
