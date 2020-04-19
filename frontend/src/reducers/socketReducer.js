const initialState = { socket: null, wsOnline: false };

/**
 * perform a task depending on the action dispatched
 *
 * @property {*} state the current state that is being held by the redux store
 * @property {*} action contains the type and payload
 */
const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      /* store websocket in state.socket */
      console.log("connecting to ws");
      return { ...state, socket: action.payload, wsOnline: true };
    case "REGISTER":
      console.log(action.payload);
      state.socket.send(JSON.stringify(action.payload));
      return state;
    case "SEND_MESSAGE":
      /* TODO handle sending on socket messages */
      return state;
    case "RESPONSE" /* TODO decipher and handle response here!! */:
      console.log("handle the response bro pls");
      console.log(action.payload);
      return state;
    case "DISCONNECTED":
      return { ...state, socket: action.payload, wsOnline: false };
    case "SETSERVER":
      return { ...state, socket: action.payload };
    default:
      return state;
  }
};

export default socketReducer;
