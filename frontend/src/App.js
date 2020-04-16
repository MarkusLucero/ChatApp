import React, {useState} from "react";
import "./assets/main.css";
import Container from "./components/Container/Container";

const App = () => {

  const [isLoading, setIsLoading] = useState(true); 
  const [message, setMessage] = useState("Hejsan"); 

  var ws = new WebSocket("ws://localhost:8080/websocket");
  ws.onopen= ()=>{console.log("connection established!"); setIsLoading(false)};

  ws.onmessage = (data) =>{console.log(data.data)};
    
  const sendMessage = () => {
    if(!isLoading){
    ws.send(message);
    }
  };


  return (
    <div className="App">
      <Container />
      <button onClick ={sendMessage}className="bg-black text-white">Send</button>
    </div>
  );
};

export default App;
