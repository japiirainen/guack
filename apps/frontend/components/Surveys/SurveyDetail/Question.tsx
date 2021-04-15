import { Box, IconButton } from '@material-ui/core'
import Delete from '@material-ui/icons/Delete'
import React from 'react'
import { Draggable } from 'react-beautiful-dnd'

import * as Survey from '@/Survey'
import { onSuccess, PromiseExit, WithLoading } from '@/data'

import { TextFieldWithEditor } from '../components'

export function Question({
   deleteQuestion,
   index,
   question: q,
   updateTitle,
}: {
   question: Survey.SurveyQuestion
   index: number
   updateTitle: WithLoading<(newTitle: string) => PromiseExit>
   deleteQuestion: WithLoading<() => void>
}) {
   return (
      <Draggable draggableId={index.toString()} index={index}>
         {provided => (
            <Box
               ref={provided.innerRef}
               {...provided.draggableProps}
               {...provided.dragHandleProps}
               display={'flex'}
            >
               <Box flexGrow={1}>
                  <TextFieldWithEditor
                     loading={updateTitle.loading}
                     initialValue={q.title}
                     onChange={(title, onSuc) => {
                        updateTitle(title).then(onSuccess(onSuc))
                     }}
                  >
                     {q.title}
                  </TextFieldWithEditor>
               </Box>
               <Box>
                  <IconButton disabled={deleteQuestion.loading} onClick={deleteQuestion}>
                     <Delete />
                  </IconButton>
               </Box>
            </Box>
         )}
      </Draggable>
   )
}
