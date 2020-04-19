
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

/* action functions */
export function connect(ws) {
  return { type: CONNECT, payload: ws };
}  

/* action functions */
export function disconnected(data) {
  return { type: DISCONNECTED, payload: data };
}  

export function login({ values }) {
  return {
    type: LOGIN,
    payload: {
      action: "login",
      username: values.Username,
      password: values.Password,
    },
  };
}

export function register({ values }) {
  return {
    type: REGISTER,
    payload: {
      action: "register",
      username: values.Username,
      password: values.Password,
    },
  };
}

export function loginSuccess({}){}
export function loginFailure({}){}

export function response(data) {
  return { type: RESPONSE, payload: data};
}

export function setServer(server){
  return {type: SET, payload: server};
}

