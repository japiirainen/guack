import { Request, Response } from '@guack/client/Surveys/GetSurvey'

import * as SurveyContext from './SurveysContext'

export const handle = (_: Request) => SurveyContext.find(_.id)

export { Request, Response }
