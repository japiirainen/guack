import qs from 'querystring'

import React from 'react'
import { BrowserRouter, Redirect, Route, Switch } from 'react-router-dom'

import Surveys from './Surveys'
import logo from './logo.svg'
import './App.css'

function getQueryParam(search: qs.ParsedUrlQuery, param: string) {
   const v = search[param]
   if (Array.isArray(v)) {
      return v[0]
   }
   return v ?? null
}

export function App() {
   return (
      <div className="App">
         <header className="App-header">
            <img src={logo} className="App-logo" alt="logo" />
         </header>
         <section id="main">
            <BrowserRouter>
               <Switch>
                  <Route
                     path="/:category"
                     render={({
                        location,
                        match: {
                           params: { category },
                        },
                     }) => {
                        const pars = qs.parse(location.search.slice(1))

                        return (
                           <Surveys
                              category={category}
                              order={getQueryParam(pars, 'order')}
                              orderDirection={getQueryParam(pars, 'orderDirection')}
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
