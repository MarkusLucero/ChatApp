import React from "react";
import { BrowserRouter, Route } from "react-router-dom";
import LandingPage from "../LandingPage/LandingPage";

const Container = () => {
  return (
    <BrowserRouter>
      <Route path="/start" component={LandingPage} />
    </BrowserRouter>
  );
};

export default Container;
