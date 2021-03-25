import dotenv from 'dotenv'
dotenv.config()
import { getEnvOrElse } from './utils'

export const config = {
   application: {
      port: getEnvOrElse('PORT', '4200'),
      name: getEnvOrElse('NAME', 'amole'),
   },
   DATABASE_URL: getEnvOrElse(
      'DATABASE_URL',
      'postgres://guackamole:salaisuus@localhost:5432/guack-pg'
   ),
}
