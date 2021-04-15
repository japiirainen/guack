import * as O from '@effect-ts/core/Option'
import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import { Box, Card } from '@material-ui/core'
import Today from '@material-ui/icons/Today'
import React from 'react'
import { Draggable } from 'react-beautiful-dnd'
import styled from 'styled-components'

import * as S from '../../../Survey'
import { ClickableMixin } from '../components'

function makeQuestionsCount(qs: S.Survey['questions']) {
   if (qs.length === 0) {
      return <>0</>
   }
   return <>{qs.length} questions</>
}

export const StyledCard = styled(Card)`
   ${ClickableMixin}
`

// const State = styled.span<StateMixinProps>`
//    ${StateMixin}
// `

export const Survey = ({
   index,
   setSelectedSurveyId,
   survey: s,
}: {
   survey: S.Survey
   index: number
   setSelectedSurveyId: (id: UUID) => void
}) => {
   return (
      <Draggable draggableId={s.id} index={index}>
         {provided => (
            <StyledCard
               ref={provided.innerRef}
               {...provided.draggableProps}
               {...provided.dragHandleProps}
               onClick={() => setSelectedSurveyId(s.id)}
            >
               <Box display={'flex'}>
                  <Box flexGrow={1} display={'flex'}>
                     <div>
                        {O.isSome(s.startDate) && (
                           <>
                              <Today /> Start day -&nbsp;
                           </>
                        )}
                        {makeQuestionsCount(s.questions)}
                        &sbsp;
                     </div>
                  </Box>
               </Box>
            </StyledCard>
         )}
      </Draggable>
   )
}
