const loginState = { loginSuccess: false, username: null, loginResponse : null };


/**
 * loginReducer handles actions of login
 * 
 * @param {object} state
 * @property {bool} loginSuccess true if login was successfull, otherwise false
 * @property {string} username holds the username that we successfully logged in with
 * @property {string} loginResponse holds the response from the http request
 * @param {object} action 
 * @property {string} type what type of action it is
 * @property {object} payload contains nescessary data for that particular action type
 * 
 * @returns updated state
 */
const loginReducer = (state = loginState, action) => {
  switch (action.type) {
    case "SUCCESS": {
      return {...state, loginSuccess: true, username : action.payload.username, loginResponse: action.payload.response};
    }
    case "RESET": {
      return {...state, loginSuccess: false, username: null, loginResponse : null }
    }
    case "FAILURE": {
        /* Failure simply returns loginState as false */
      return state;
    }
    default:
      return state;
  }
};

export default loginReducer;

