import { make, AType, EType, opaque } from '@effect-ts/morphic'
import { Survey } from '@guack/types'

const RequestPath_ = make(F =>
   F.interface({
      id: F.uuid(),
   })
)
const Request_ = make(F => F.intersection(RequestPath_(F))())
export interface Request extends AType<typeof Request_> {}
export interface RequestE extends EType<typeof Request_> {}
export const Request = Object.assign(opaque<RequestE, Request>()(Request_), {
   Path: RequestPath_,
})

export const Response = make(F => F.nullable(Survey(F)))
export type Response = AType<typeof Response>
export type ResponseE = EType<typeof Response>
