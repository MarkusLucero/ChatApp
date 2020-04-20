import socketReducer from "./socketReducer.js";
import loginReducer from "./loginReducer.js";
import { combineReducers} from "redux";



const rootReducer = combineReducers ({
    loginState : loginReducer,
    socketState: socketReducer,
})


export default rootReducer;