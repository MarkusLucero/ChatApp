import React from "react";
import "../../assets/main.css";

/**
 * Message is the component of each specific message item
 * @property message current state of the message 
 * @returns a div containing the message with appropriate styles
 */
const Message = ({ message }) => {
  const date = new Date(); 
  const month = date.getMonth() +1; 
  const timestamp = date.getFullYear().toString()+"-"+month.toString()+"-"+date.getDate().toString();
  const timestampTime = message.timestamp.split(' ')[1].slice(0,5); 
  const timestampDate = message.timestamp.split(' ')[0];
  const stampWithoutSec = message.timestamp.slice(0,15);


  return (
    <div
      style={{ backgroundColor: "#2C2F33" }}
      className="flex h-auto mb-1 text-white flex-col"
    >
      <h1 className="font-medium">
        {message.username + "  "}
    <span className="font-light text-xs text-right">{timestamp === timestampDate ? ("Today at " + timestampTime ): (stampWithoutSec)}</span>
      </h1>
      <p className="font-light"> {message.message}</p>
    </div>
  );
};

export default Message;