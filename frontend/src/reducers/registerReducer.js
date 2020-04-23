const registerState = {}

/**
 * registerReducer handles actions of register
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
const registerReducer = (state = registerState, action) => {
  switch (action.type) {
    case "SUCCESS": {
        /*TODO: Add access token to the action payload and add it to localStorage from here ?   */
      return {...state, loginSuccess: true, username : action.payload.username, loginResponse: action.payload.response};
    }
    case "FAILURE": {
        /* Failure simply returns loginState as false */
      return state;
    }

    /* TODO case for register? */

    default:
      return state;
  }
};

export default registerReducer;
