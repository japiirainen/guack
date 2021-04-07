import * as O from '@effect-ts/core/Option'
import * as Lens from '@effect-ts/monocle/Lens'
import { AType, EType, make, opaque } from '@effect-ts/morphic'

import { makeUuid, NonEmptyString } from './shared'

// export const SurveyQuestion = t.type({
//    id: t.number,
//    title: t.string,
//    surveyId: t.number,
//    type: t.string,
//    choices: t.array(t.string),
//    createdOn: date,
// })
