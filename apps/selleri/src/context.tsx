import * as SurveyClient from '@guack/client'
import { ApiConfig } from '@guack/client'
import { Fiber, pipe } from '@effect-ts/core'
import * as T from '@effect-ts/core/Effect'
import { pretty } from '@effect-ts/core/Effect/Cause'
import * as L from '@effect-ts/core/Effect/Layer'
import { Exit } from '@effect-ts/system/Exit'
import { Semaphore } from '@effect-ts/system/Semaphore'
import { StyledEngineProvider } from '@material-ui/core'
import { StylesProvider } from '@material-ui/core/styles'
import AdapterDateFns from '@material-ui/lab/AdapterDateFns'
import LocalizationProvider from '@material-ui/lab/LocalizationProvider'
import { datumEither } from '@nll/datum'
import React, { createContext, ReactNode, useContext, useEffect, useMemo } from 'react'
