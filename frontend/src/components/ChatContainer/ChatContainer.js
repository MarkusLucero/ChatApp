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
   * Trigger the search of the searchTerm in the actuall focused chat
   * event parameter is displayed only to prevent he windows default action when pressing a submit button
   * @param event the event object of the window
   */
  const handleSearchSubmit = (event) => {
    /* TODO Actually search for chat messages containing the text in searchTerm and only allow that
       to be displayed in the chat
    */
    console.log(searchTerm);
    setSearchTerm("");
    event.preventDefault();
  };

  /* get my username from redux store */
  const myUsername = useSelector((state) => state.socketState.username);

  /* State and callback functions for Chat and ChatInput */

  const listOfDms = useSelector((state) => state.socketState.listOfDms);

  const rightChat = (list) => {
    for (const chat of list) {
      if (chat.chatName === focusedChat) {
        return chat.messages;
      }
    }
    return [];
  };

  const [messages, setMessages] = React.useState([]);

  /*This is the state used when typing a new message*/
  const [newMessage, setNewMessage] = React.useState("");

  /**
   * Set the messages state by updating it with the "newMessage" message
   * @param event the event object of the window
   */
  const sendMessage = (event) => {
    event.preventDefault();
    setMessages([...messages, { message: newMessage, username: myUsername }]);
    dispatch(
      actions.sendMessage({
        message: newMessage,
        username: myUsername,
        chatName: focusedChat,
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

  /* 
    will render on mount - dependency list holds focusedChat so whenever that changes we will fire everything inside useEffect
    when focusedChat changes we change the messages that are displayed. 
    
    If state of listOfDms changes we will refire this aswell displaying the new chat messages 
    ( this hapens when we get a response from someone or when we send a message and update listOfDms)
    */
  React.useEffect(() => {
    console.log("list of dms");
    console.log(listOfDms);
    console.log("fookus my man fookus");
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
