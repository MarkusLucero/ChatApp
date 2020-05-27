import React from "react";
import "./assets/main.css";
import Container from "./components/Container/Container";
import { createStore } from "redux";
import { Provider } from "react-redux";
import rootReducer from "./reducers";


/**
 * App is the root node in our component tree. holds the container component and surrounds it with the Provider tag
 * which gives all components down the tree access to the redux store
 * 
 * @returns {JSX} a div holding on to the container component
 */
const App = () => {

  /**
   * store holds all our state created with our combined reducers inside rootReducer
   * @property {param} rootReducer the reducer that combines all reducers
   * @param {tools} __REDUX_DEVTOOLS_EXTENSION__ download extension in your browser for overview of state in store
   * 
   * @returns the store object which contains all state from rootReducer
   */
  const store = createStore(rootReducer,
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
