/*
 initialState is the state object that we send into the redux store and can manipulate with the reducers
 */
const initialState = {
  socket: null,
  wsOnline: false,
  socketServer: null,
  firstWelcome: null,
  username: null,
  listOfDms: null,
  listOfFriends: null,
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
 * @property {array} listOfMessages holds all chatroom objects
 * @property {array} listOfFriends holds all friends usernames
 * @property {string} username holds the username of logged in user
 * @param {object} action contains the type and payload
 * @property {string} action.type what kind of action should the reducer do
 * @property {object} action.payload check actions.js for what it may contain
 *
 * @returns updated state
 */
const socketReducer = (state = initialState, action) => {
  switch (action.type) {
    case "CONNECT":
      /* store websocket in state.socket */
      console.log("connecting to ws");
      var firstWelcome;
      if (state.firstWelcome === null) {
        firstWelcome = true;
      } else {
        firstWelcome = state.firstWelcome ? true : false;
      }
      return {
        ...state,
        socket: new WebSocket(state.socketServer),
        wsOnline: true,
        firstWelcome: firstWelcome,
      };
    case "ADDFRIEND":
      return {
        ...state,
        listOfFriends: state.listOfFriends.concat(action.payload.username),
      };
    case "CHAT_REQUEST":
      console.log(action.payload);
      state.socket.send(JSON.stringify(action.payload));
      return state;
    case "RESPONSE":
      /* if data is ack or welcome there's nothning we need to do */
      if (action.payload.data === "Welcome" || action.payload.data === "ACK") {
        return state;

        /* first response */
      } else if (action.payload.action === "login") {
        /* need to respond to socket with action = login, username, and magictoken to establish connection */
        state.socket.send(JSON.stringify(action.payload));

        /* TODO TODO
          This if ann else cases handles the fact that we accidentely get 2 login responses from backend right now
          not to trigger useEffect in ChatContainer we have this case here.. TODO remove it after backend fixes it
        */
        if (state.firstWelcome === false) {
          console.log("Duplicate welcome login response...");
          return state;
        } else {
          console.log("first welcome login response");
          return {
            ...state,
            firstWelcome: false, // no longer first welcome..
            /* 
            TESTING -- TODO - HARDCODED the login object that we should get in accordance with doc
            in accordance with doc it should be a response with action "init_login" but we will do 
            it here right now

                        each user gets 2 hard coded chatrooms on login
                        each user gets 4 hard coded friends on login
                        user get's its username  ( not hardcoded it comes from action.payload.username )

            */
            listOfDms: [
              {
                chatName: "Skooben",
                chatID: "1a",
                messages: [{ message: "test", username: "Markipie" }],
                members: [],
                creator: "",
              },
              {
                chatName: "Grabbarna Grus",
                chatID: "2a",
                messages: [
                  { message: "Axel mitt sexdjur?", username: "Anton" },
                  { message: "axel e arg på dig", username: "Axel" },
                ],
                members: [],
                creator: "",
              },
            ],
            listOfFriends: ["Skooben", "Markipie", "Mustafa", "Pallerkan"],
            username: action.payload.username,
          };
        }
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
          case "chat_request":
            /* insert a new chat object to listOfDms */
            if (parsedData.status === "ok") {
              return {
                ...state,
                listOfDms: [
                  ...state.listOfDms,
                  /* newly inserted object */
                  {
                    chatName: parsedData.chat_name,
                    chatID: parsedData.chat_id,
                    messages: [],
                    members: parsedData.members,
                    creator: parsedData.creator,
                  },
                ],
              };
            }
            return state;
          default:
            return state;
        }
      }

    case "DISCONNECTED":
      return {
        ...state,
        socket: action.payload,
        wsOnline: false,
        firstWelcome: null,
        listOfDms: null,
        listOfFriends: null,
        username: null,
      };

    case "SETSERVER":
      return { ...state, socketServer: action.payload };

    case "SENDMESSAGE":
      const index = getChatIndex(state.listOfDms, action.payload.chat_id);

      if (index !== -1) {
        const msgObject = action.payload;
        state.socket.send(JSON.stringify(msgObject));

        /* update listOfDms in state */
        var updateListOfDms = state.listOfDms;
        updateListOfDms[index].messages.push({
          message: msgObject.message,
          username: msgObject.user_id,
        });
       
        return {
          ...state,
          listOfDms: updateListOfDms,
        };
      }
      return state;
    default:
      return state;
  }
};

export default socketReducer;
