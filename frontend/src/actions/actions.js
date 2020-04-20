
/**
 * Contains actions that can be called by a dispatcher
 * 
 */

 /* action types */
const CONNECT = "CONNECT";
const LOGIN = "LOGIN";
const RESPONSE = "RESPONSE"
const REGISTER = "REGISTER";
const DISCONNECTED = "DISCONNECTED";
const SET = "SETSERVER";
const SUCCESS = "SUCCESS";
const FAILURE = "FAILURE";
const SENDMESSAGE = "SENDMESSAGE";

export function sendMessage(data) {
  return {
    type: SENDMESSAGE,
    payload: {
        action: "send_message",                        // type of action 
        chat_id: 0001,                                       // chat id . kan skippas nu för alla hamnar i samma chat i prototypen
        user_id: username ,
        message: data.message , 
        timestamp: null
    },
  };
}
/* action functions */
export function connect(ws) {
  return { type: CONNECT, payload: ws };
}  

/* action functions */
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

export function register( values ) {
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

export function loginSuccess({response}){
  return { type: SUCCESS, payload: response}
}
export function loginFailure({response}){
  return {type: FAILURE, payload: response}
}

export function response(data) {
  return { type: RESPONSE, payload: data};
}

export function setServer(server){
  return {type: SET, payload: server};
}

