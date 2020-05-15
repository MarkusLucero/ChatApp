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
const LOGOUT = "LOGOUT";
const RESET = "RESET";
const CREATE_THREAD = "CREATE_THREAD";
const ADD_THREADS = "ADD_THREADS";
const ADD_COMMENT = "ADD_COMMENT";
const ADD_REPLY = "ADD_REPLY";
const RESET_LAST_SEEN = "RESET_LAST_SEEN";


export function sendMessage(data) {
  return {
    type: SENDMESSAGE,
    payload: {
      action: "send_message", // type of action
      chat_id: data.chatID,
      user_id: data.username,
      message: data.message,
      timestamp: null,
    },
  };
}

export function addComment(data) {
  return {
    type: ADD_COMMENT,
    payload: {
      thread_id: data.thread_id,
      user_id: data.username,
      comment: data.comment,
      reply: data.reply,
    },
  };
}
export function addReply(data) {
  return {
    type: ADD_REPLY,
    payload: {
      thread_id: data.thread_id,
      user_id: data.username,
      comment: data.comment,
      reply: {
        user_id: data.reply.user_id,
        comment: data.reply.comment,
      },
    },
  };
}

export function addFriend(data) {
  return { type: ADDFRIEND, payload: { username: data.username } };
}

export function connect() {
  return { type: CONNECT, payload: null };
}

export function disconnected(data) {
  return { type: DISCONNECTED, payload: data };
}

export function logOut(data) {
  return { type: LOGOUT };
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

export function startChat(values) {
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
export function loginSuccess(data) {
  return { type: SUCCESS, payload: data };
}
export function createThread(values) {
  return {
    type: CREATE_THREAD,
    payload: {
      serverName: values.server.serverName,
      thread_id: null,
      username: values.user,
      root_post: { root_header: values.summary, root_cooment: values.details },
      timestamp: null,
      comments: [],
    },
  };
}
export function addThreads(data) {
  return { 
    type: ADD_THREADS,
    payload: {
      threads: data.threads, 
    }
  };
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

export function resetLoginState() {
  return { type: RESET };
}

export function resetLastSeen(data) {
  return { type: RESET_LAST_SEEN, payload:data };
}
