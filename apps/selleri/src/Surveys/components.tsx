import { NonEmptyString } from '@guack/types/shared'
import { IconButton, TextField, TextFieldProps } from '@material-ui/core'
import Favorite from '@material-ui/icons/Favorite'
import FavoriteBorder from '@material-ui/icons/FavoriteBorder'
import React, { useState, useRef, useEffect, MouseEventHandler } from 'react'
import styled, { css } from 'styled-components'

import { onSuccess, PromiseExit } from '../infrastructure/data'

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

export interface StateMixinProps {
   state?: 'warn' | 'error' | null
}

export function StateMixin({ state }: StateMixinProps) {
   return css`
      color: ${state === 'warn' ? 'yellow' : state === 'error' ? 'red' : 'inherit'};
   `
}
