const initialState = { socket: null, response: null, test: "ok" };

const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      //don't reconnect if it's already connected
      return { ...state, socket: action.payload };
    case "LOGIN":

      return state;
    case "REGISTER":

      return state;
    case "SEND_MESSAGE":

      return state; 
    case "RESPONSE":  /* TODO decipher and handle response here!! */
      console.log("handle the response bro pls");
      console.log(action.payload);
      return state;
    default:
      return state;
  }
};

export default socketReducer;
