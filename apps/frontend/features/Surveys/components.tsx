import { NonEmptyString } from '@guack/types/shared'
import { TextField, TextFieldProps } from '@material-ui/core'
import React, { useState, useEffect, MouseEventHandler, useRef } from 'react'
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

export function TextFieldWithEditor({
   children,
   initialValue,
   loading,
   multiline,
   onChange,
   renderTextField: Field = TextField,
}: {
   children: React.ReactNode
   onChange: (note: string, onSuc: () => void) => void
   loading: boolean
   initialValue: string
   multiline?: boolean
   renderTextField?: (p: TextFieldProps) => JSX.Element
}) {
   const [text, setText] = useState(initialValue)
   const [editing, setEditing] = useState(false)
   const editor = useRef<HTMLInputElement | undefined>(undefined)
   useEffect(() => {
      if (editing) {
         editor?.current?.focus()
      }
   }, [editing])
   useEffect(() => {
      setText(initialValue)
   }, [initialValue])

   const submit = () => onChange(text, () => setEditing(false))

   return editing ? (
      <Field
         size="small"
         multiline={multiline}
         inputRef={editor}
         value={text}
         disabled={loading}
         onKeyDown={multiline ? undefined : evt => evt.key === 'Enter' && submit()}
         onBlur={submit}
         onChange={evt => setText(evt.target.value)}
      />
   ) : (
      <Clickable style={{ display: 'inline' }} onClick={() => setEditing(true)}>
         {children}
      </Clickable>
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
