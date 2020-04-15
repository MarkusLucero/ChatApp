import React, { useState } from "react";
import "../../assets/main.css";
import Message from "./Message";
/**
 * Chat holds the layout of all messages and the input/sent
 *
 * @returns a div containing all messages aswell as input for new messages
 */
const Chat = () => {
  /*All old messages, these are hardcoded. The self field is if you've sent the message or not */
  const [messages, setMessages] = useState([
    { message: "Hej", self: true },
    { message: "Tjena!", self: false },
  ]);
  /*This is the state used when typing a new message*/
  const [newMessage, setNewMessage] = useState("");
  /**
   * Set the message state by updating it with the "newMessage" message
   * @param  e the event object of the window
   */
  const sendMessage = (e) => {
    e.preventDefault();
    setMessages((prev) => [...prev, { message: newMessage, self: true }]);
    setNewMessage("");
    document.getElementById("message").reset();
    console.log(newMessage);
  };

  return (
    <div className="bg-gray-800 h-screen">
      <div id="messages">
        {/* Loop through the state messages and print a message component for each message  */}
        {messages.map((message, index) => (
          <div key={index}>
            <Message message={message} />
          </div>
        ))}
      </div>
      <div id="input&send" className="absolute inset-x-0 bottom-0 h-10 ">
        <form id="message" className="h-full" onSubmit={sendMessage}>
          <input
            className="bg-blue-200 w-3/4 h-full"
            type="text"
            onChange={(e) => setNewMessage(e.target.value)}
            placeholder="New message..."
          />
          <button className="h-full w-1/4 bg-blue-100" type="submit">
            Send
          </button>
        </form>
      </div>
    </div>
  );
};

export default Chat;
