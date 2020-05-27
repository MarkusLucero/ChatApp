import React, { useRef, useEffect } from "react";
import { BrowserRouter, Route, Switch, Redirect } from "react-router-dom";
import LandingPage from "../LandingPage/LandingPage";
import Login from "../Login/Login";
import Register from "../Register/Register";

import * as actions from "../../actions/actions";
import { useDispatch, useSelector } from "react-redux";

/**
 * Container is the component that makes sure to route to specific component on start page also
 * handles the listener / connection to the websocket
 * @returns returns the actual component that should be rendered depending on pathname
 */
const Container = () => {
  /**
   * HANDLING AND INITIALISATION OF WEBSOCKET EFFECTS
   */

  /* ws is the websocket that we are connected to */
  const ws = useSelector((state) => state.socketState.socket);

  /* used in useEffect to check if disconnect is by our own accord */
  const wsOnline = useSelector((state) => state.socketState.wsOnline);

  /* websocket url - will contain address when we call SETSERVER action */
  const url = useSelector((state) => state.socketState.socketServer);

  const dispatch = useDispatch();

  /* used for first login repsonse to the socket*/
  const loginResponse = useSelector((state) => state.loginState.loginResponse);
  /* used for first login repsonse to the socket*/
  const username = useSelector((state) => state.loginState.username);

  // used to keep a mutable ref object - in this case the websocket which will not change automatically during a re-render
  const wsRef = useRef();
  wsRef.current = ws;

  /* 
    used only for checking if we have made our first response for socket
    We need to do this because the first response has to look a certain way
    for the webserver to authenticate us
  */
  var firstWelcome = useSelector((state) => state.socketState.firstWelcome);

  useEffect(() => {
    /* if current prop of ref is null and ws url is set -> initialize new websocket connection */
    if (!wsRef.current && url !== null) {
      dispatch(actions.connect());
    }

    /* socket is online  */
    if (wsRef.current != null) {
      /* listening on websocket */
      wsRef.current.onmessage = (msg) => {
        //first time we need to establish an authentication with server
        if (firstWelcome === true) {
          dispatch(
            actions.response({
              action: "login",
              username: username,
              magictoken: loginResponse,
            })
          );
        } else {
          dispatch(actions.response(msg));
        }
      };

      /* our websocket disconnected */
      wsRef.current.onclose = () => {
        if (wsOnline) {
          /* trigger a reconnect */
          console.log("reconnect to new ws");
          dispatch(actions.connect());
        } else {
          /* disconnect the ws */
          console.log("ws disconnected");
          dispatch(actions.disconnected(null));
        }
      };
    }
  }, [ws, url, firstWelcome]);

  /* a variable which checks wether we've successfully logged in or not taken from redux store */
  const loginSuccess = useSelector((state) => state.loginState.loginSuccess);

  return (
    <BrowserRouter>
      <Switch>
        <Route path="/" exact>
          {loginSuccess ? <Redirect to="/start" /> : <Login />}
        </Route>
        <Route path="/start">
          {loginSuccess ? <LandingPage /> : <Redirect to="/" />}
        </Route>
        <Route path="/register" render={(props) => <Register {...props} />} />
      </Switch>
    </BrowserRouter>
  );
};

export default Container;
