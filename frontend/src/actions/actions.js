
/**
 * Contains actions that can be called by a dispatcher
 * 
 */

 /* action types */
const CONNECT = "CONNECT";
const LOGIN = "LOGIN";
const RESPONSE = "RESPONSE"
const REGISTER = "REGISTER";

/* action functions */
export function connect(ws) {
  return { type: CONNECT, payload: ws };
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

export function response(data) {
  return { type: RESPONSE, payload: data};
}

