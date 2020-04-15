import React from "react";
import "./assets/main.css";
import LandingPage from "./components/LandingPage/LandingPage";
/* import Register from "./components/Register/Register";
 */ import Login from "./components/Register/Login";

const App = () => {
  return (
    <div className="App">
      {/* TODO LandingPage will go through container component and not directly to App - this is only for visual testing purposes*/}
      {/*      <LandingPag     */}
      {/*       <Register />
       */}
      <Login />
    </div>
  );
};

export default App;
