import React from "react";
import "../../assets/main.css";

/**
 * Message is the component of each specific message item
 * @property message current state of the message
 * @returns a div containing the message with appropriate styles
 */
const Message = ({ message }) => {

  return (
    <div
      style={{ backgroundColor: "#2C2F33" }}
      className="flex h-auto mb-1 text-white flex-col"
    >
      <h1 className="font-medium">
        {" "}
        {message.username + "  "}
        {/* TODO -- SET REAL TIMESTAMPS */}
        <span className="font-light text-xs text-right">today...</span>
      </h1>
      {/* TODO -- SET REAL MESSAGES */}
      <p className="font-light"> {message.message}</p>
    </div>
  );
};

export default Message;