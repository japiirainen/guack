import dotenv from 'dotenv'
dotenv.config()
import { getEnvOrElse } from './utils'

export const config = {
   application: {
      PORT: getEnvOrElse('PORT', '4200'),
      NAME: getEnvOrElse('NAME', 'amole'),
      HOST: getEnvOrElse('HOST', '127.0.0.1'),
   },
   DATABASE_URL: getEnvOrElse(
      'DATABASE_URL',
      'postgres://guackamole:salaisuus@localhost:5432/guack-pg'
   ),
}
