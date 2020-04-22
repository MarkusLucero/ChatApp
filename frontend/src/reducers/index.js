import socketReducer from "./socketReducer.js";
import loginReducer from "./loginReducer.js";
import { combineReducers} from "redux";



/**
 * rootReducer is the reducer that we use as argument for createStore --
 * here we can combine all different reducers that we create
 * 
 * @property {object} loginState holds state by loginReducer
 * @property {object} socketState holds state by socketState
 * 
 * @returns {object} a reducer that combines all argument reducers
 */
const rootReducer = combineReducers ({
    loginState : loginReducer,
    socketState: socketReducer,
})


export default rootReducer;