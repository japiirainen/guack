import * as t from 'io-ts'
import { date } from 'io-ts-types'
import { Response } from 'express'
import { CustomError } from 'ts-custom-error'
import { v4 as uuidv4 } from 'uuid'

export const Survey = t.type({
   id: t.number,
   active: t.boolean,
   startDate: date,
   expires: date,
   createdOn: date,
})

export const SurveyQuestion = t.type({
   id: t.number,
   title: t.string,
   surveyId: t.number,
   type: t.string,
   choices: t.array(t.string),
   createdOn: date,
})

export type Survey = t.TypeOf<typeof Survey>
export type SurveyQuestion = t.TypeOf<typeof SurveyQuestion>

export type QuestionType = 'CheckBox' | 'LinearScale' | 'MultiChoice' | 'TextField' | 'EmailField'

export interface ApplicationError extends Error {
   /** What HTTP status code to respond with */
   status: number
   /**
    * Error code to return in the response,
    * used to conceal implementation details (error messages, stack traces)
    * while still providing a code that can be traced to specific logs
    * */
   code: string
   /** Whether the error should be logged, true for unexpected errors, false for bussiness logic errors */
   log: boolean
}

export const processError = (res: Response) => (err: ApplicationError): void => {
   if (!err.code && !err.status) {
      err = new UnexpectedError(err.message)
   }
   if (err.log) {
      console.error(err.message)
   }
   res.status(err.status).json({ code: err.code })
}

// General errors
class UnexpectedError extends CustomError implements ApplicationError {
   status = 500
   code = uuidv4()
   log = true
}

export class InvalidRequest extends CustomError implements ApplicationError {
   status = 500
   code = 'InvalidRequest'
   log = false
}

export class ValidationFailed extends CustomError implements ApplicationError {
   status = 500
   code = 'ValidationFailed'
   log = false
}

export class DBError extends CustomError implements ApplicationError {
   status = 500
   code = 'DBError'
   log = true
}
