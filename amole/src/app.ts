import cors from 'cors'
import express, { Express } from 'express'
import morgan from 'morgan'
import { createPool } from './infrastrucure/db'
import { initEnv } from './infrastrucure/appEnv'

export const createApp = async (): Promise<Express> => {
   const prerequisites = [createPool(), Promise.resolve()] as const
   const [pool] = await Promise.all(prerequisites)
   const app = express()

   const v1Routes = express.Router()

   app.use(morgan('dev'))
      .use(
         cors({
            credentials: true,
            origin: 'http://localhost:4000',
         })
      )
      .use(express.json())
      .get('/health', (_, res) =>
         pool
            ? res.status(200).json({ status: 'healthy' })
            : res.status(503).json({ status: 'unavailable' })
      )
      .use(initEnv(pool))
      .use('/api', v1Routes)

   return app
}
