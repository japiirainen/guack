import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import { Response, Request } from '@guack/client/Surveys/CreateSurvey'
import { Survey } from '@guack/types'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   pipe(
      T.effectTotal(() => Survey.create({ ..._ })),
      T.tap(SurveyContext.add),
      T.map(s => ({ id: s.id }))
   )

export { Request, Response }
