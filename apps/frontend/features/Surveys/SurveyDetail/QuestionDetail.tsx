import * as A from '@effect-ts/core/Collections/Immutable/Array'
import * as T from '@effect-ts/core/Effect'
import { flow, pipe } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { Box, Button, Checkbox, IconButton, TextField } from '@material-ui/core'
import ArrowRight from '@material-ui/icons/ArrowRight'
import Delete from '@material-ui/icons/Delete'
import { DatePicker, DateTimePicker } from '@material-ui/lab'
import { datumEither } from '@nll/datum'
import React from 'react'
import { Droppable, DragDropContext } from 'react-beautiful-dnd'
import styled from 'styled-components'

import * as Todo from '@/Todo'
import {
   Completable,
   FavoriteButton,
   Field,
   StateMixin,
   StateMixinProps,
   TextFieldWithEditor,
} from '@/components'
import { useServiceContext } from '@/context'
import { memo, onSuccess, withLoading } from '@/data'
import { constEmptyString } from '@/utils'

import { useDeleteTask, useTaskCommandsResolved } from '../data'

import { Step } from './Step'
