const registerState = {}

/**
 * registerReducer handles actions of register
 * 
 * @param {object} state TODO
 * @param {object} action TODO
 * 
 * @returns updated state
 */
const registerReducer = (state = registerState, action) => {
  switch (action.type) {
    case "SUCCESS": { //TODO this is same as login right now... not good
      
      //TODO
      return state;
    }
    case "FAILURE": {
        /* Failure simply returns loginState as false */
      //TODO
      return state;
    }

    /* TODO case for register? */

    default:
      return state;
  }
};

export default registerReducer;
