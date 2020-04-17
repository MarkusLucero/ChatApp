import React, { useRef, useEffect } from "react";
import { BrowserRouter, Route, Switch } from "react-router-dom";
import LandingPage from "../LandingPage/LandingPage";
import Login from "../Login/Login";
import Register from "../Register/Register";

import * as actions from "../../actions/actions";
import { useDispatch, useSelector } from "react-redux";

/**
 * Container is the component that makes sure to route to specific component on start page
 * TODO more functionality when we can use user objects etc
 * @returns returns the actuall component that should be rendered depending on pathname
 */
const Container = () => {

  /**
   * HANDLING AND INITIALISATION OF WEBSOCKET EFFECTS
   */

  const ws = useSelector((state) => state.socket) /* used in useEffect to check if we need to refire useEffect() */
  const wsOnline = useSelector((state) => state.wsOnline); /* used in useEffect to check if disconnect is by our own accord */

  // so we can dispatch actions to the store
  const dispatch = useDispatch();

  // used to keep a mutable ref object - in this case the websocket
  const wsRef = useRef();
  /* websocket url */
  const url = "ws://localhost:8080/websocket";

  // Initiates the websocket client on mount (everything in useEffect is called on mount - like created/mounted in Vue)
  useEffect(() => {
    // if current prop of ref is null -> initialize new websocket connection (this happens first time)
    if (!wsRef.current) {
      wsRef.current = new WebSocket(url);
      dispatch(actions.connect(wsRef.current)); // add the ref to the redux store
    }

    /* listening on messages received - response handled by the reducer?? */
    wsRef.current.onmessage = (msg) => {
      console.log(msg.data);
      dispatch(actions.response(msg));
    };
    /* our websocket disconnected */
    wsRef.current.onclose = () => {
      if (wsOnline) {
        /* trigger a reconnect */
        console.log("reconnect to new ws");
        wsRef.current = new WebSocket(url);
        dispatch(actions.connect(wsRef.current)); // add the ref to the redux store
      } else {
        /* disconnect the ws */
        console.log("ws disconnected");
        dispatch(actions.disconnected(null));
      }
    };
  },[ws]); /* dependency list includes ws - when ws is changed we refire the useEffect hook */

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
