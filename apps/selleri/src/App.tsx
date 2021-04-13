import qs from 'querystring'

import React from 'react'
import { BrowserRouter, Redirect, Route, Switch } from 'react-router-dom'

import { Surveys } from './Surveys'

function getQueryParam(search: qs.ParsedUrlQuery, param: string) {
   const v = search[param]
   if (Array.isArray(v)) {
      return v[0]
   }
   return v ?? null
}

export function App() {
   return (
      <div>
         <header></header>
         <section id="main">
            <BrowserRouter>
               <Switch>
                  <Route
                     path="/:category"
                     render={({
                        // location,
                        match: {
                           params: { category },
                        },
                     }) => {
                        const pars = qs.parse(location.search.slice(1))
                        console.log(category)
                        console.log(getQueryParam(pars, 'order'))

                        return (
                           <Surveys
                           // category={category}
                           // order={getQueryParam(pars, 'order')}
                           // orderDirection={getQueryParam(pars, 'orderDirection')}
                           />
                        )
                     }}
                  />
                  <Route path="/">
                     <Redirect to="/surveys" />
                  </Route>
               </Switch>
            </BrowserRouter>
         </section>
      </div>
   )
}
