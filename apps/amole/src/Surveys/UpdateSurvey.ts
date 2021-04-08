import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'
import { Lens } from '@effect-ts/monocle'
import { Response, Request } from '@guack/client/Surveys/UpdateSurvey'
import { Survey } from '@guack/types'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) =>
   pipe(
      SurveyContext.get(_.id),
      T.map(
         Survey.lens['|>'](Lens.props('active', 'expires', 'startDate', 'title'))['|>'](
            Lens.modify(s => ({ ...s, ..._, updatedAt: new Date() }))
         )
      ),
      T.tap(SurveyContext.add),
      T.asUnit
   )

export { Request, Response }
