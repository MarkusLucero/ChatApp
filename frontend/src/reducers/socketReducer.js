/*
 initialState is the state object that we send into the redux store and can manipulate with the reducers
 */
const initialState = {
  socket: null,
  wsOnline: false,
  socketServer: null,
  firstWelcome: null,
  listOfDms: null,
};

/**
 * helper function used by action type SENDMESSAGE to locate the correct chat id
 * @param {array} list containing all direct messages of the user
 * @param {string} chatName used to identify the correct dm object
 *
 * @returns the chatID of the dm object
 */
const getChatID = (list, chatName) => {
  let index = 0;
  for (const chat of list) {
    if (chat.chatName === chatName) {
      return [chat.chatID, index];
    }
    index++;
  }
  return [null, -1]; // this line will only be reached if we give an invalid chatName
};
/**
 * helper function used by action type "RESPONSE" and action "send_message" to locate the correct dm object index
 * @param {array} list containing all direct messages of the user
 * @param {string} chatID used to identify the correct dm object
 *
 * @returns the index of the correct dm object
 */
const getChatIndex = (list, chatID) => {
  let index = 0;
  for (const chat of list) {
    if (chat.chatID === chatID) {
      return index;
    }
    index++;
  }
  return -1; // this line will only be reached if we give an invalid chatID
};

/**
 * performs a task depending on the action.type dispatched - acts more as a middleware for socket handling
 *
 * !!! Reducers shouldn't do sideeffects which this one does - consider this one more as a middleware than a real reducer
 *
 * @param {object} state the current state that is being held by the redux store
 * @property {object} socket contains the actual socket that we communicate with
 * @property {bool} wsOnline is true when the socket is online (used to handle disconnects/time outs)
 * @property {string} socketServer the url of server that we create a websocket with
 * @property {bool} firstWelcome used to know if we've made our first repsonse to socket after login  (TODO maybe better way)
 * @property {object} latestMessage holds the latest message obj sent by another user. (TODO ONLY FOR THE PROTOTYPE!!!)
 *  @property {array} listOfMessages holds all chatroom objects TODO!
 * @param {object} action contains the type and payload
 * @property {string} action.type what kind of action should the reducer do
 * @property {object} action.payload check actions.js for what it may contain
 * @property {object} action.identifier used for identifying something inside state object .. eg: chatID of specific chat
 *
 * @returns updated state
 */
const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      /* store websocket in state.socket */
      console.log("connecting to ws");
      return {
        ...state,
        socket: new WebSocket(state.socketServer),
        wsOnline: true,
        firstWelcome: true,
      };
    case "CHAT_REQUEST": 
      state.socket.send(JSON.stringify(action.payload));
      return state;
    case "RESPONSE":
      /* if data is ack or welcome there's nothning we need to do */
      if (action.payload.data === "Welcome" || action.payload.data === "ACK") {
        return state;
      } else if (action.payload.action === "login") {
        /* first response */
        console.log(state);
        state.socket.send(JSON.stringify(action.payload));

        return {
          ...state,
          firstWelcome: false,
          /* 
          TESTING -- each user gets 2 hard coded chatrooms on login
                     perhaps something like this will work on login.. 
                     database sends user a list of all its chat rooms with the stored messages in correct order..  
          */
          listOfDms: [
            {
              chatName: "Skooben",
              chatID: "1a",
              messages: [{ message: "test", username: "Markipie" }],
            },
            {
              chatName: "Grabbarna Grus",
              chatID: "2a",
              messages: [
                { message: "Axel mitt sexdjur?", username: "Anton" },
                { message: "axel e arg på dig", username: "Axel" },
              ],
            },
          ],
        };
      } else {
        const parsedData = JSON.parse(action.payload.data);

        /* We respond differently depending on the action/type of received data */
        switch (parsedData.action) {
          case "send_message":
            /* add the new msg object to the right dm object */
            const index = getChatIndex(state.listOfDms, parsedData.chat_id);
            return {
              ...state,
              listOfDms: [
                ...state.listOfDms,
                state.listOfDms[index].messages.push({
                  message: parsedData.message,
                  username: parsedData.user_id,
                }),
              ],
            };
          default:
            return state;
        }
      }

    case "DISCONNECTED":
      return {
        ...state,
        socket: action.payload,
        wsOnline: false,
        firstWelcome: true,
      };

    case "SETSERVER":
      return { ...state, socketServer: action.payload };

    case "SENDMESSAGE":
      const [chatID, index] = getChatID(state.listOfDms, action.identifier);

      if (chatID !== null && index !== -1) {
        const msgObject = { ...action.payload, chat_id: chatID };
        state.socket.send(JSON.stringify(msgObject));
        /* update listOfDms in state */
        return {
          ...state,
          listOfDms: [
            ...state.listOfDms,
            state.listOfDms[index].messages.push({
              message: msgObject.message,
              username: msgObject.user_id,
            }),
          ],
        };
      }
      return state; 
    default:
      return state;
  }
};

export default socketReducer;
