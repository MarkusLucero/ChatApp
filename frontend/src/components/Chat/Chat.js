import React from "react";
import "../../assets/main.css";
import Message from "./Message";
/**
 * Chat holds the layout of all messages and the input/sent
 * @property messages the state containing list of chat messages
 * @returns a div containing all messages 
 */
const Chat = ({ messages }) => {

  /* Using ref to makes sure that we scroll to end of chat area if it overflows */
  const messagesEndRef = React.useRef(null);
  const scrollToBottom = () => {
    messagesEndRef.current.scrollIntoView({ behavior: "smooth" });
  };
  React.useEffect(scrollToBottom, [messages]);

  
  return (
    <div id ="chat-area" className=" h-screen75 w-full pl-2 pr-2 mt-10 overflow-y-scroll">
      <div id="messages">
        {/* Loop through the state messages and print a message component for each message  */}
        {messages.map((message, index) => (
          <div key={index}>
            <Message message={message} />
          </div>
        ))}
      </div>
      <div ref={messagesEndRef} />
    </div>
  );
};

export default Chat;