{-# LANGUAGE BlockArguments       #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module InterfaceAdapters.RestService where

import           Data.Time.Calendar     (Day)
import qualified Domain.SurveyDomain    as DomS (Survey, SurveyId)
import           Numeric.Natural
import           Polysemy
import           Polysemy.Error
import           Polysemy.Trace         (Trace)
import           Servant
import qualified UseCases.SurveyUseCase as UCS (SurveyError, SurveyPersistence,
                                                createSurvey, deleteSurvey,
                                                fetchSurvey)

type SurveyAPI =
        "survey" :> Summary "retrieve a survey (SurveyId -> Survey)"
                 :> Get '[ JSON] DomS.Survey -- GET /survey

   :<|> "survey" :> Summary "create a new survey"
                 :> ReqBody '[ JSON] DomS.Survey
                 :> Post '[ JSON] () -- POST /survey

   :<|> "survey" :> Summary "delete a survey"
                 :> ReqBody '[JSON] DomS.SurveyId
                 :> Delete '[JSON] () -- DELETE /survey


surveyServer :: (Member UCS.SurveyPersistence r, Member (Error UCS.SurveyError) r,
                 Member Trace r) => ServerT SurveyAPI (Sem r)
surveyServer =
            UCS.fetchSurvey -- GET /survey
    :<|>    UCS.createSurvey -- POST /survey
    :<|>    UCS.deleteSurvey -- DELETE /survey

reservationAPI :: Proxy SurveyAPI
reservationAPI = Proxy
