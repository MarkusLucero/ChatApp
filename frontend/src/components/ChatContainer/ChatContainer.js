import React from "react";
import "../../assets/main.css";
import SearchBar from "../SearchBar/SearchBar";
import Chat from "../Chat/Chat";
import ChatInput from "../ChatInput/ChatInput";
import * as actions from "../../actions/actions";
import { useDispatch, useSelector } from "react-redux";

/**
 * ChatContainer holds the layout of the focused view of a selected chat
 * @property {string} focusedChat the name of the chat that we are currently focusing on
 * @returns a div containing the SearchBar, Chat and ChatInput components
 */
const ChatContainer = ({ focusedChat }) => {
  const dispatch = useDispatch();

  /* State and callback functions for the SearchBar */

  const [searchTerm, setSearchTerm] = React.useState("");

  /**
   * Set the state of searchTerm to the value of the input field when that changes
   * @param event the event object of the window
   */
  const handleSearchInput = (event) => {
    setSearchTerm(event.target.value);
  };

  /**
   * TODO Trigger the search of the searchTerm in the actuall focused chat
   * @param event the event object of the window
   */
  const handleSearchSubmit = (event) => {
    /* TODO Actually search for chat messages containing the text in searchTerm and only allow that
       to be displayed in the chat
    */
    console.log(searchTerm);

    /* OBS OBS !! IF YOU WANT TO TRY OUT THE PYTHON SEARCH API UNCOMMENT THE FETCH STATEMENT BELOW */
    // Send the same request
    /*     fetch("http://localhost:5000/search", {
      // Specify the method
      method: "POST",
      // A JSON payload
      body: JSON.stringify({
        "search_term": searchTerm,
      }),
    })
      .then(function (response) {
        return response.json(); //parse result as JSON
      })
      .then(function (json) {
        console.log("Search results: ");
        console.log(json); // Here’s our JSON object
      }); */
    setSearchTerm("");
    event.preventDefault();
  };

  const myUsername = useSelector((state) => state.socketState.username);
  const listOfDms = useSelector((state) => state.socketState.listOfDms);

  /**
   * Check for the right DM object chatID wich matches the focusedChat prop
   * @param {array} list the list containing Dm objects
   * @return {array} array containing the list of message objects from the corresponding DM object
   */
  const rightChat = (list) => {
    for (const chat of list) {
      if (chat.chatID === focusedChat) {
        return chat.messages;
      }
    }
    return [];
  };

  const [messages, setMessages] = React.useState([]);

  const [newMessage, setNewMessage] = React.useState("");

  /**
   * Set the messages state setting it to include the "newMessage" message
   * @param event the event object of the window
   */
  const sendMessage = (event) => {
    event.preventDefault();
    setMessages([...messages, { message: newMessage, username: myUsername }]);
    dispatch(
      actions.sendMessage({
        message: newMessage,
        username: myUsername,
        chatID: focusedChat,
      })
    );
    setNewMessage("");
  };

  /**
   * Set the newMessage state to whatever value is in event target
   * @param event the event object of the window
   */
  const handleMessage = (event) => {
    setNewMessage(event.target.value);
  };

  /* HANDLING THE DISPLAY OF NEW MESSAGES AND NEW FOCUSED CHAT */
  
  React.useEffect(() => {
    if (listOfDms != null) {
      setMessages(rightChat(listOfDms));
    }
  }, [focusedChat, listOfDms]);

  return (
    <div className="flex flex-col content-center focused-view-custom-bg">
      <SearchBar
        id="search-chat"
        value={searchTerm}
        onButtonClick={handleSearchSubmit}
        onInputChange={handleSearchInput}
      />

      <Chat messages={messages} />
      {focusedChat ? (
        <ChatInput
          message={newMessage}
          handleInputChange={handleMessage}
          handleButtonClick={sendMessage}
        />
      ) : null}
    </div>
  );
};

export default ChatContainer;
