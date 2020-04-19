const loginState = { loginSuccess: false };

const loginReducer = (state = loginState, action) => {
  switch (action.type) {
    case "SUCCESS": {
        /*TODO: Add access token to the action payload and add it to localStorage from here  */
      return {...state, loginSuccess: true};
    }
    case "FAILURE": {
        /* Failure simply returns loginState as false */
      return state;
    }
    default:
      return state;
  }
};
