/*
 initialState is the state object that we send into the redux store and can manipulate with the reducers
 */
const initialState = {
  socket: null,
  wsOnline: false,
  socketServer: null,
  firstWelcome: null,
  magicToken: null,
  username: null,
  listOfDms: null,
  listOfFriends: null,
  server: null,
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
    case "ADD_COMMENT":
      /* This will fetch us the correct thread */
      for (const thread of state.server.listOfThreads) {
        if (thread.id === action.payload.thread_id) {
          thread.comments.push({
            user_id: action.payload.user_id,
            comment: action.payload.comment,
            reply: action.payload.reply,
          });
        }
      }
      /*       state.socket.send(JSON.stringify(action.payload));
       */
      return state;
    case "ADD_REPLY":
      for (const thread of state.server.listOfThreads) {
        if (thread.id === action.payload.thread_id) {
          thread.comments.push({
            user_id: action.payload.user_id,
            comment: action.payload.comment,
            reply: action.payload.reply,
          });
        }
      }
      /*       state.socket.send(JSON.stringify(action.payload));
       */ return state;
    case "CREATE_THREAD":
      state.socket.send(JSON.stringify(action.payload));
      return state;
    case "ADDFRIEND":
      return {
        ...state,
        listOfFriends: state.listOfFriends.concat(action.payload.username),
      };
    case "CHAT_REQUEST":
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
          This if and else cases handles the fact that we accidentely get 2 login responses from backend right now
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
            magicToken: action.payload.magictoken.magic_token,
            /* 
            TESTING -- TODO - HARDCODED the login object that we should get in accordance with doc
            in accordance with doc it should be a response with action "init_login" but we will do 
            it here right now

            */
            listOfDms: [],
            listOfFriends: [],
            username: action.payload.username,
            /* set hardcoded server oject with name of GLOBAL, empty thread list and member list with only urself */
            server: {
              serverName: "0",
              serverInformation:
                "This is the global server that everyone joins. Make threads, comment and be happy peeps.",
              listOfThreads: [
                {
                  rootPost: {
                    rootHeader: "Hjälp med linux!",
                    rootComment: "Hej, har nån bra koll på mint??",
                  },
                  username: "Skooben",
                  timestamp: "2020-01-10",
                  comments: [],
                  id: "1",
                },
                {
                  rootPost: {
                    rootHeader: "Knarka i helgen?",
                    rootComment:
                      "Tjena, skulle vilja knarka med någon i helgen. Sugen?",
                  },
                  username: "Rövpannan",
                  timestamp: "2019-12-24",
                  comments: [],
                  id: "2",
                },
              ],
              members: [action.payload.username],
            },
          };
        }
      } else {
        const parsedData = JSON.parse(action.payload.data);

        /* We respond differently depending on the action/type of received data */
        switch (parsedData.action) {
          case "init_login":
            const listOfDms = parsedData.list_of_dms.map((obj) => {
              return { ...obj, sinceLastSeen: 0 }; /* used for notifications */
            });
            return {
              ...state,
              listOfDms: listOfDms,
              listOfFriends: parsedData.list_of_friends,
              username: parsedData.user_id,
            };
          case "send_message":
            /* add the new msg object to the right dm object */
            const index = getChatIndex(state.listOfDms, parsedData.chat_id);
            return {
              ...state,
              listOfDms: [
                ...state.listOfDms,
                state.listOfDms[index].sinceLastSeen++,
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
                    sinceLastSeen: 0 /* used for notifications */,
                  },
                ],
              };
            }
            return state;
          case "create_thread":
            return {
              ...state,
              server: {
                ...state.server,
                listOfThreads: [
                  ...state.server.listOfThreads,
                  {
                    rootPost: {
                      rootHeader: parsedData.root_post.root_header,
                      rootComment: parsedData.root_post.root_comment,
                    },
                    username: parsedData.username,
                    timestamp: parsedData.timestamp,
                    comments: parsedData.commentList,
                    id: parsedData.thread_id,
                  },
                ],
              },
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
    case "LOGOUT":
      /* RESET STATE */
      return {
        ...state,
        socket: null,
        wsOnline: false,
        socketServer: null,
        firstWelcome: null,
        magicToken: null,
        username: null,
        listOfDms: null,
        listOfFriends: null,
      };
    case "RESET_LAST_SEEN":
      /* Reset the since last seen counter to 0 */
      console.log(action.payload)
      const i = getChatIndex(state.listOfDms, action.payload.chatID);
      return {
        ...state,
        listOfDms: [
          ...state.listOfDms,
          state.listOfDms[i].sinceLastSeen = 0,
        ],
      };

    default:
      return state;
  }
};

export default socketReducer;
