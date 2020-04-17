import React, { useRef, useEffect } from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import LandingPage from "../LandingPage/LandingPage";
import Login from "../Login/Login";
import Register from "../Register/Register";

import * as actions from "../../actions/actions";
import { useDispatch} from "react-redux";

/**
 * Container is the component that makes sure to route to specific component on start page
 * TODO more functionality when we can use user objects etc
 * @returns returns the actuall component that should be rendered depending on pathname
 */
const Container = () => {

  // so we can dispatch actions to the store
  const dispatch = useDispatch();

  // used to keep a mutable ref object - in this case the websocket
  const wsRef = useRef();

  // Initiates the websocket client on mount 
  useEffect(() => {
    // if current prop of ref is null -> initialize new websocket connection
    if (!wsRef.current) {
      wsRef.current = new WebSocket("ws://localhost:8080/websocket");
      dispatch(actions.connect(wsRef.current)); // add it to the redux store 
    }
    wsRef.current.onmessage = (msg) => {
      console.log(msg.data);
      dispatch(actions.response(msg));   // handle the response
    };
  });

  return (
    <BrowserRouter>
      <Switch>
        <Route path="/" exact render={(props) => <Login {...props} />} />
        <Route path="/start" render={(props) => <LandingPage {...props} />} />
        <Route path="/register" render={(props) => <Register {...props} />} />
      </Switch>
    </BrowserRouter>
  );
};

export default Container;
