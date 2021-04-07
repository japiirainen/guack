import { config } from './infrastrucure/config'
import { createApp } from './app'
import { info } from './infrastrucure/log'

createApp().then(app =>
   app.listen(config.application.port, () =>
      info(`${config.application.name} is listening on ${config.application.port}`)()
   )
)
