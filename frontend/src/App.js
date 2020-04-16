import React, { useState } from "react";
import "./assets/main.css";
import Container from "./components/Container/Container";

const App = () => {
  return (
    <div className="App">
      <Container />
      <button onClick={sendMessage} className="bg-black text-white">
        Send
      </button>
    </div>
  );
};

export default App;
