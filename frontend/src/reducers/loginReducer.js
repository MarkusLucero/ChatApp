const loginState = { loginSuccess: false, username: null, loginResponse : null };

const loginReducer = (state = loginState, action) => {
  switch (action.type) {
    case "SUCCESS": {
        /*TODO: Add access token to the action payload and add it to localStorage from here  */
      return {...state, loginSuccess: true, username : action.payload.username, loginResponse: action.payload.response};
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

