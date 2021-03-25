import * as TE from 'fp-ts/TaskEither'
import { Pool, PoolClient, PoolConfig } from 'pg'
import { migrate } from 'postgres-migrations'
import { DBError } from '../../../common/src/error'
import { config } from './config'
import { error, info } from './log'

export const createPool = async (): Promise<Pool | null> => {
   const pgConf: PoolConfig = {
      connectionString: config.DATABASE_URL,
      ssl: false,
   }
   const pool = new Pool(pgConf)
   try {
      const client = await pool.connect()
      try {
         info('running migrations...')()
         await migrate({ client }, 'sql')
      } finally {
         await client.release()
      }
   } catch (e) {
      error(e)()
      error('DB connection failed')()
      return null
   }
   return pool
}

export const withConn = <T>(
   pool: Pool,
   f: (conn: PoolClient) => Promise<T>
): TE.TaskEither<DBError, T> =>
   TE.tryCatch(
      async () => {
         const client = await pool.connect()
         try {
            return await f(client)
         } catch (e) {
            throw new DBError(e.message)
         } finally {
            await client.release()
         }
      },
      () => new DBError('db error')
   )

export const inTransaction = <T>(
   pool: Pool,
   f: (conn: PoolClient) => Promise<T>
): TE.TaskEither<DBError, T> =>
   TE.tryCatch(
      async () => {
         const client = await pool.connect()
         try {
            await client.query(`BEGIN;
                                SET SESSION CHARASTERICS AS TRANSACTION READ WRITE;`)
            const result = await f(client)
            await client.query('COMMIT;')
            return result
         } catch (e) {
            await client.query('ROLLBACK;')
            throw new DBError(e.message)
         } finally {
            await client.release()
         }
      },
      () => new DBError('db error')
   )
