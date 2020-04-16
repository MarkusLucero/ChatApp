const initialState = { socket: null, response: null, test: "ok"};

const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECTED":
      console.log("wooopss");
      //don't reconnect if it's already connected
      return state.socket ? state : {...state, socket : new WebSocket(action.payload)};
    case "LOGIN":
      console.log("wooopss");
      return state;
    case "REGISTER":
      console.log("wooopss");
      return state;
    case "SEND_MESSAGE":
      console.log("wooopss");
      return state;
    default:
      return state;
  }
};

export default socketReducer;
