const initialState = { socket: null, test: "ok" };


/**
 * perform a task depending on the action dispatched
 * 
 * @property {*} state the current state that is being held by the redux store
 * @property {*} action contains the type and payload
 */
const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      //don't reconnect if it's already connected
      return { ...state, socket: action.payload };
    case "LOGIN":
      /* Handle LOGIN -- store user somewhere? */
      console.log(action.payload);
      state.socket.send(action.payload);
      return;
    case "REGISTER":
      console.log(action.payload);
      state.socket.send(action.payload);
      return state;
    case "SEND_MESSAGE":
      /* TODO handle sending on socket messages */
      return state;
    case "RESPONSE" /* TODO decipher and handle response here!! */:
      console.log("handle the response bro pls");
      console.log(action.payload);
      return state;
    default:
      return state;
  }
};

export default socketReducer;
