import React from "react";
import "../../assets/main.css";

/**
 * ChatContainer holds the layout of the focused view of a selected chat
 * 
 * @returns a div containing the SearchBar, Chat and ChatInput components
 */
const ChatContainer = () => {
  return (
    <div className="flex flex-col items-center">
      {/* TODO These divs will be replaced by SearchBar, Chat and ChatInput components respectively hence the id:s*/}
      <div id="search-bar" className="bg-blue-200">search</div>
      <div id="chat" className="bg-blue-400">chat</div>
      <div id="chat-input" className="bg-blue-600 ">chat-input</div>
    </div>
  );
};

export default ChatContainer;