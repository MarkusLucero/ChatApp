import React from "react";
import "../../assets/main.css";
import SearchBar from "../SearchBar/SearchBar";

/**
 * ChatContainer holds the layout of the focused view of a selected chat
 *
 * @returns a div containing the SearchBar, Chat and ChatInput components
 */
const ChatContainer = () => {
  /* State and callback functions for the SearchBar */
  const [searchTerm, setSearchTerm] = React.useState("");

  /**
   * Set the state of searchTerm to the value of the input field when that changes
   * @param event the event object of the window
   *
   */
  const handleSearchInput = (event) => {
    setSearchTerm(event.target.value);
  };

  /**
   * Trigger the search of the searchTerm in the actuall focused chat
   * event parameter is displayed only to prevent he windows default action when pressing a submit button
   * @param event the event object of the window
   *
   */
  const handleSearchSubmit = (event) => {
    /* TODO Actually search for chat messages containing the text in searchTerm and only allow that
       to be displayed in the chat
    */
    console.log(searchTerm);
    event.preventDefault();
  };

  return (
    <div className="flex flex-col items-center">
      <SearchBar
        id="search-chat"
        value={searchTerm}
        onButtonClick={handleSearchSubmit}
        onInputChange={handleSearchInput}
      />
      {/* TODO These divs will be replaced by Chat and ChatInput components respectively hence the id:s*/}
      <div id="chat" className="bg-blue-400">
        chat
      </div>
      <div id="chat-input" className="bg-blue-600 ">
        chat-input
      </div>
    </div>
  );
};

export default ChatContainer;
