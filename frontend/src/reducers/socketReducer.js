/**
 * initialState is the state object that we send into the redux store and can manipulate with the reducers
 * @property {object} socket contains the actual socket that we communicate with
 * @property {bool} wsOnline is true when the socket is online (used to handle disconnects/time outs)
 * @property {string} socketServer the url of server that we create a websocket with
 * @property {bool} firstWelcome used to know if we've made our first repsonse to socket after login  (TODO maybe better way)
 * @property {object} latestMessage holds the latest message send by another user. (TODO ONLY FOR THE PROTOTYPE!!!)
 *
 */
const initialState = {
  socket: null,
  wsOnline: false,
  socketServer: null,
  firstWelcome: null,
  latestMessage: null,
};

/**
 * performs a task depending on the action.type dispatched
 *
 * @property {object} state the current state that is being held by the redux store
 * @property {object} action contains the type and payload
 * @property {string} action.type what kind of action should the reducer do
 * @property {object} action.payload check actions.js for what it may contain
 *
 */
const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      /* store websocket in state.socket */
      console.log("connecting to ws");
      return {
        ...state,
        socket: action.payload,
        wsOnline: true,
        firstWelcome: true,
        latestMessage: null,
      };

    case "REGISTER":
      console.log(action.payload);
      state.socket.send(JSON.stringify(action.payload));
      return state;
      
    case "RESPONSE":
      /* if data is ack or welcome there's nothning we need to do */
      if (action.payload.data === "Welcome" || action.payload.data === "ACK") {
        return state;
      }else if ( action.payload.action === "login")/* first response */{
        state.socket.send(JSON.stringify(action.payload));
        return { ...state, firstWelcome: false };
      }
      else {
        const parsedData = JSON.parse(action.payload.data);
        console.log(parsedData);
        /* We respond differently depending on the action/type of received data */
        switch (parsedData.action) {
          case "send_message":
            return {
              ...state,
              latestMessage: {
                message: parsedData.message,
                username: parsedData.user_id,
              },
            };
          default:
            return state;
        }
      }

    case "DISCONNECTED":
      return { ...state, socket: action.payload, wsOnline: false };

    case "SETSERVER":
      return { ...state, socketServer: action.payload };

    case "SENDMESSAGE":
      state.socket.send(JSON.stringify(action.payload));
      return state;

    default:
      return state;
  }
};

export default socketReducer;
