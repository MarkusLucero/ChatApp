import React from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import LandingPage from "../LandingPage/LandingPage";
import Login from "../Login/Login";
import Register from "../Register/Register";

/**
 * Container is the component that makes sure to route to specific component on start page
 * TODO more functionality when we can use user objects etc
 * @returns returns the actuall component that should be rendered depending on pathname
 */
const Container = () => {

  return (
    <BrowserRouter>
      <Switch>
        <Route path="/" exact render= {(props)=> <Login {...props} />} />
        <Route path="/start" render={(props)=> <LandingPage {...props} />} />
        <Route path="/register" render ={(props)=> <Register {...props} />} />
      </Switch>
    </BrowserRouter>
  );
};

export default Container;