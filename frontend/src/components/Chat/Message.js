import React from "react";
import "../../assets/main.css";

/**
 * Message is the component of each specific message item
 * @property message current state of the message
 * @returns a div containing the message with appropriate styles
 */
const Message = ({ message }) => {
  return (
    <div className={"flex h-10 mb-1" + (message.self ? ' justify-end' : ' justify-start')}>
      <div className={"h-full p-2 rounded-full static center" + (message.self ? " bg-blue-200" : " bg-gray-200")}>
        {message.message}
      </div>
    </div>
  );
};

export default Message;
