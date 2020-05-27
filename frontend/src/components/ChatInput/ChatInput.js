import React from "react";
import "../../assets/main.css";

/**
 * ChatInput is the message input component
 * @property message the state of the current message
 * @property handleInputChange callback function to activate when value of input changes
 * @property handleButtonClick callback function to activate when clicking the button/ pressing enter
 * @returns a div containing the input area of the message
 */
const ChatInput = ({ message, handleInputChange, handleButtonClick }) => {
  return (
    <div className="self-center m-1 mt-auto w-full">
      <form id="message" className="flex justify-center focused-view-custom-bg">
        <input
        className="w-full  m-1  h-16 input-box-custom-bg text-white shadow appearance-none py-2 px-3 focus:outline-none"
          type="text"
          onChange={handleInputChange}
          value={message}
          placeholder="New message..."
        />
        <button onClick={handleButtonClick}></button>
      </form>
    </div>
  );
};

export default ChatInput;