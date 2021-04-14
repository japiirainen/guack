import { Request, Response } from '@guack/client/Surveys/SetSurveyOrder'
import * as T from '@effect-ts/core/Effect'
import { pipe } from '@effect-ts/core/Function'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) => pipe(SurveyContext.setOrder(_.order), T.asUnit)

export { Request, Response }
