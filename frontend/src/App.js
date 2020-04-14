import React from "react";
import "./assets/main.css"
import LandingPage from "./components/LandingPage/LandingPage"

const App = () => {
  return <div className="App">
    
     {/* TODO LandingPage will go through container component and not directly to App - this is only for visual testing purposes*/}
     <LandingPage/>
  </div>;
};

export default App;
