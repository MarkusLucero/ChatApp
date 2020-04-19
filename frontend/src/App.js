import React from "react";
import "./assets/main.css";
import Container from "./components/Container/Container";
import socketReducer from "./reducers/socketReducer";
import loginReducer from "./reducers/loginReducer";
import { combineReducers, createStore } from "redux";
import { Provider } from "react-redux";

const App = () => {
  const store = createStore(
    combineReducers(socketReducer, loginReducer),
    window.__REDUX_DEVTOOLS_EXTENSION__ && window.__REDUX_DEVTOOLS_EXTENSION__()
  );

  return (
    <div className="App">
      <Provider store={store}>
        <Container />
      </Provider>
    </div>
  );
};

export default App;
