import type { Branded } from '@effect-ts/core/Branded'
import { constVoid } from '@effect-ts/core/Function'
import * as Sy from '@effect-ts/core/Sync'
import { make, FastCheckURI, AType, DecoderURI, EncoderURI, opaque } from '@effect-ts/morphic'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import { decode } from '@effect-ts/morphic/Decoder'
import { flow } from '@effect-ts/system/Function'
import { v4 } from 'uuid'

export function makeUuid() {
   return v4() as UUID
}

const MIN = 1

export const isNonEmptyString = (v: String): v is Branded<string, NonEmptyStringBrand> =>
   v.length >= MIN

/**
 * A string of Min 1 and max 256KB characters
 */
const NonEmptyString0 = make(F =>
   F.refined(F.string(), isNonEmptyString, {
      name: 'NonEmptyString',
      conf: {
         [FastCheckURI]: (_c, fc) =>
            fc.module
               .string({ minLength: MIN })
               .map(x => x as Branded<string, NonEmptyStringBrand>),
      },
   })
)

export const NonEmptyString = Object.assign(NonEmptyString0, {
   parse: flow((s: string) => s, decode(NonEmptyString0)),
})

export interface NonEmptyStringBrand {
   readonly NonEmptyString: unique symbol
}

export type NonEmptyString = AType<typeof NonEmptyString>

const defaultVoid = Sy.succeed(constVoid())
const defaultVoidThunk = () => defaultVoid
const Void_ = make(F =>
   F.unknown({
      conf: {
         [DecoderURI]: codec => codec.with(defaultVoidThunk),
         [EncoderURI]: () => ({ encode: defaultVoidThunk }),
      },
   })
)
export type Void = void

export const Void = opaque<Void, Void>()(Void_ as any)