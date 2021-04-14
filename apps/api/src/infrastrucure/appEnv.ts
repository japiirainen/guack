import { NextFunction, Request, Response } from 'express'
import { Pool } from 'pg'

export interface AppEnv {
   pg: Pool
}

export const initEnv = (pool: Pool | null) => (
   req: Request,
   _: Response,
   next: NextFunction
) => {
   req.env = { pool } as never
   next()
}
