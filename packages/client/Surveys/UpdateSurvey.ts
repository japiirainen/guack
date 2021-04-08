import { make, AType, EType, opaque } from '@effect-ts/morphic'
import { ExpireDate, SurveyId, StartDate } from '@guack/types'
import { NonEmptyString, Void } from '@guack/types/shared'

const Request_ = make(F =>
   F.both(
      {
         id: SurveyId(F),
      },
      {
         title: NonEmptyString(F),
         active: F.boolean(),
         expires: ExpireDate(F),
         startDate: StartDate(F),
      }
   )
)
export interface Request extends AType<typeof Request_> {}
export interface RequestE extends EType<typeof Request_> {}
export const Request = opaque<RequestE, Request>()(Request_)

export const Response = Void
export type Response = AType<typeof Response>
export type ResponseE = EType<typeof Response>
