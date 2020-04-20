import React from "react";
import "./assets/main.css";
import Container from "./components/Container/Container";
import { createStore } from "redux";
import { Provider } from "react-redux";
import rootReducer from "./reducers";

const App = () => {
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
