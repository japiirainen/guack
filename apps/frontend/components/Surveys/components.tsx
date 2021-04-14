import { NonEmptyString } from '@guack/types/shared'
import { TextField, TextFieldProps } from '@material-ui/core'
import React, { useState, useEffect, MouseEventHandler } from 'react'
import styled, { css } from 'styled-components'

import { onSuccess, PromiseExit } from '../../data'

export const Clickable = styled.div`
   ${ClickableMixin}
`

export interface ClickableMixinProps<El> {
   onClick?: MouseEventHandler<El>
}

export function ClickableMixin<El>({ onClick }: ClickableMixinProps<El>) {
   return (
      onClick &&
      css`
         cursor: pointer;
      `
   )
}

export const Field = ({
   onChange,
   state,
   ...rest
}: {
   onChange: (t: NonEmptyString) => PromiseExit
   state?: unknown
} & Omit<TextFieldProps, 'onChange'>) => {
   const [text, setText] = useState('')
   const clearText = () => setText('')

   useEffect(() => {
      clearText()
   }, [state])

   return (
      <TextField
         value={text}
         onChange={evt => setText(evt.target.value)}
         onKeyPress={evt => {
            evt.charCode === 13 &&
               text.length &&
               onChange(text as NonEmptyString).then(onSuccess(clearText))
         }}
         {...rest}
      />
   )
}

export interface StateMixinProps {
   state?: 'warn' | 'error' | null
}

export function StateMixin({ state }: StateMixinProps) {
   return css`
      color: ${state === 'warn' ? 'yellow' : state === 'error' ? 'red' : 'inherit'};
   `
}
