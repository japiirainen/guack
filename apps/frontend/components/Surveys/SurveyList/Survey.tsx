import { flow } from '@effect-ts/core/Function'
import * as O from '@effect-ts/core/Option'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import * as TodoClient from '@guack/client'
import * as A from '@guack/types/ext/Array'
import { Box, Card, Checkbox } from '@material-ui/core'
import Alarm from '@material-ui/icons/Alarm'
import CalendarToday from '@material-ui/icons/CalendarToday'
import Today from '@material-ui/icons/Today'
import { datumEither } from '@nll/datum'
import React, { useEffect, useState } from 'react'
import { Draggable, Droppable, DragDropContext } from 'react-beautiful-dnd'
import styled from 'styled-components'

import { useServiceContext } from '../../../context'
import { memo } from '../../../data'
import * as Survey from '../Survey'
import { updateSurveyIndex } from '../Survey'
import { StateMixinProps, StateMixin, ClickableMixin } from '../components'
import { useModifySurveys, useSurveyCommands } from '../data'
import { withLoading } from '../utils'

function makeQuestionsCount(qs: Survey.Survey['questions']) {
   if (qs.length === 0) {
      return <>0</>
   }
   return <>{qs.length} questions</>
}

const StyledCard = styled(Card)`
   ${ClickableMixin}
`

const State = styled.span<StateMixinProps>`
   ${StateMixin}
`

export const Survey = ({
   index,
   setSelectedSurveyId,
   survey: s,
}: {
   survey: Survey.Survey
   index: number
   setSelectedSurveyId: (id: UUID) => void
}) => {
   const { findResult, updateResult } = useSurveyCommands(s.id)
   const isRefreshingSurvey = datumEither.isRefresh(findResult)
   const isUpdatingSurvey = datumEither.isPending(updateResult) || isRefreshingSurvey

   const { runPromise } = useServiceContext()

   const toggle
}
