import { UUID } from '@effect-ts/morphic/Algebra/Primitives'
import * as SurveyClient from '@guack/client'
import * as A from '@guack/types/ext/Array'
import React, { useEffect, useState } from 'react'
import { Droppable, DragDropContext } from 'react-beautiful-dnd'
import styled from 'styled-components'

import * as S from '@/Survey'
import { useServiceContext } from '@/context'
import { memo } from '@/data'

import { useModifySurveys } from '../data'

import { StyledCard, Survey } from './Survey'

const CardList = styled.div`
   > ${StyledCard} {
      padding: 4px;
      margin-top: 8px;
      margin-bottom: 8px;
   }
`

export const SurveyList = memo(function ({
   setSelectedSurveyId,
   surveys,
}: {
   setSelectedSurveyId: (i: UUID) => void
   surveys: A.Array<S.Survey>
}) {
   const { runWithErrorLog } = useServiceContext()
   const modifySurveys = useModifySurveys()
   const [{ activeSurveys, closedSurveys }, setFilteredSurveys] = useState(() => ({
      closedSurveys: [] as A.Array<S.Survey>,
      activeSurveys: [] as A.Array<S.Survey>,
   }))

   useEffect(() => {
      setFilteredSurveys({
         activeSurveys: surveys['|>'](A.filter(x => !x.active)),
         closedSurveys: surveys['|>'](A.filter(x => x.active)),
      })
   }, [surveys])

   return (
      <DragDropContext
         onDragEnd={result => {
            const { destination } = result
            if (!destination) {
               return
            }
            const t = surveys.find(x => x.id === result.draggableId)!
            // TODO: Next section aint pretty.
            const reorder = S.updateSurveyIndex(t, destination.index)
            modifySurveys(reorder)
            const reorderSurveys = surveys['|>'](reorder)
            SurveyClient.Surveys.setSurveysOrder({
               order: A.map_(reorderSurveys, t => t.id),
            })['|>'](runWithErrorLog)
         }}
      >
         <Droppable droppableId={'surveys'}>
            {provided => (
               <CardList ref={provided.innerRef} {...provided.droppableProps}>
                  {activeSurveys.map(s => (
                     <Survey
                        survey={s}
                        index={surveys.findIndex(ot => ot === s)}
                        setSelectedSurveyId={setSelectedSurveyId}
                        key={s.id}
                     />
                  ))}
                  {provided.placeholder}
               </CardList>
            )}
         </Droppable>

         {Boolean(closedSurveys.length) && (
            <div>
               <h3>Closed</h3>
               <Droppable droppableId={'surveys-closed'}>
                  {provided => (
                     <CardList ref={provided.innerRef} {...provided.droppableProps}>
                        {closedSurveys.map(s => (
                           <Survey
                              survey={s}
                              index={surveys.findIndex(ot => ot === s)}
                              setSelectedSurveyId={setSelectedSurveyId}
                              key={s.id}
                           />
                        ))}
                        {provided.placeholder}
                     </CardList>
                  )}
               </Droppable>
            </div>
         )}
      </DragDropContext>
   )
})
