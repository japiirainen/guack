import { Response } from 'express'
import { CustomError } from 'ts-custom-error'
import { v4 as uuidv4 } from 'uuid'
import { error } from '../../amole/src/infrastrucure/log'

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
      error(err.message)()
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
