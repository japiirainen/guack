import { make, AType, EType, opaque } from '@effect-ts/morphic'
import { Void } from '@guack/types/shared'

const Request_ = make(F => F.interface({ id: F.uuid() }))
export interface Request extends AType<typeof Request_> {}
export interface RequestE extends EType<typeof Request_> {}
export const Request = opaque<RequestE, Request>()(Request_)

export const Response = Void
export type Response = AType<typeof Response>
export type ResponseE = EType<typeof Response>
