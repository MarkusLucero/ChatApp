import React from "react";
import "../../assets/main.css";
import SearchBar from "../SearchBar/SearchBar";
import Chat from "../Chat/Chat";
import ChatInput from "../ChatInput/ChatInput";

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
    setSearchTerm("");
    event.preventDefault();
  };

  
  /* State and callback functions for Chat and ChatInput */

  /*All old messages, these are hardcoded. The self field is if you've sent the message or not */
  const [messages, setMessages] = React.useState([]);
  /*This is the state used when typing a new message*/
  const [newMessage, setNewMessage] = React.useState("");
  /**
   * Set the messages state by updating it with the "newMessage" message
   * @param event the event object of the window
   */
  const sendMessage = (event) => {
    event.preventDefault();
    setMessages((prev) => [...prev, { message: newMessage, self: true }]);
    setNewMessage("");
  };
  
  /**
   * Set the newMessage state to whatever value is in event target
   * @param event the event object of the window
   */
  const handleMessage = (event) => {
    setNewMessage(event.target.value);
  };



  return (
    <div className="flex flex-col content-center focused-view-custom-bg">
      <SearchBar
        id="search-chat"
        value={searchTerm}
        onButtonClick={handleSearchSubmit}
        onInputChange={handleSearchInput}
      />

      <Chat messages={messages} />
      <ChatInput
        message = {newMessage}
        handleInputChange={handleMessage}
        handleButtonClick={sendMessage}
      />
    </div>
  );
};

export default ChatContainer;