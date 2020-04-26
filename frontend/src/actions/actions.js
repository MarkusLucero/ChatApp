/**
 * Contains actions that can be called by a dispatcher
 *
 */

/* action types */
const CONNECT = "CONNECT";
const LOGIN = "LOGIN";
const RESPONSE = "RESPONSE";
const REGISTER = "REGISTER";
const DISCONNECTED = "DISCONNECTED";
const SET = "SETSERVER";
const SUCCESS = "SUCCESS";
const FAILURE = "FAILURE";
const SENDMESSAGE = "SENDMESSAGE";
const ADDFRIEND = "ADDFRIEND";
const CHAT_REQUEST = "CHAT_REQUEST";

export function sendMessage(data) {
  return {
    type: SENDMESSAGE,
    payload: {
      action: "send_message", // type of action
      chat_id: "",
      user_id: data.username,
      message: data.message,
      timestamp: null,
    },
    identifier:
      data.chatName /* used by helper function in socketReducer to get correct chat_id */,
  };
}
export function addFriend(data) {
  return{   type: ADDFRIEND, payload:{ username: data.username }}
};


export function connect() {
  return { type: CONNECT, payload: null };
}

export function disconnected(data) {
  return { type: DISCONNECTED, payload: data };
}

export function login({ values }) {
  console.log(values);
  return {
    type: LOGIN,
    payload: {
      action: "login",
      username: values.Username,
      password: values.Password,
    },
  };
}

export function register(values) {
  console.log(values);
  return {
    type: REGISTER,
    payload: {
      action: "register",
      username: values.Username,
      password: values.Password,
    },
  };
}

export function startChat(values){
  console.log(values);
  return {
    type: CHAT_REQUEST,
    payload: {
      action: "chat_request",
      chat_name: values.chatName,
      from: values.from, 
      members: values.members,
    },
  };
}
export function loginSuccess(data){
  return { type: SUCCESS, payload: data}
}
  
export function loginFailure(data) {
  return { type: FAILURE, payload: data };
}

export function response(data) {
  return { type: RESPONSE, payload: data };
}

export function setServer(server) {
  return { type: SET, payload: server };
}

