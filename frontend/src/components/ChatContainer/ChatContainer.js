import React, { useState } from "react";
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
    setSearching(false);
    setSearchTerm(event.target.value);
  };

  /* used as a check if we are currenyly displaying searched messages */
  const [searching, setSearching] = React.useState(false);
  /**
   * TODO Trigger the search of the searchTerm in the actuall focused chat
   * @param event the event object of the window
   */
  const handleSearchSubmit = (event) => {
    setSearching(true);
    var filteredChat = [];
    messages.map((chat) => {
      if (chat.message.includes(searchTerm)) {
        filteredChat.push(chat);
      }
    });

    setMessages(filteredChat);
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

  const getChatName = (list) => {
    for (const chat of list) {
      if (chat.chatID === focusedChat) {
        return chat.chatName;
      }
    }
    return "";
  };

  const [messages, setMessages] = React.useState([]);

  const [newMessage, setNewMessage] = React.useState("");

  /**
   * Set the messages state setting it to include the "newMessage" message
   * @param event the event object of the window
   */
  const sendMessage = (event) => {
    const today = new Date();
    const month = today.getMonth() + 1; //January is 0, need to add 1
    var hours = today.getHours() - 2; //convert to UTC
    var minutes = today.getMinutes();
    if (minutes < 10) {
      minutes = "0" + minutes.toString();
    } else {
      minutes = minutes.toString();
    }
    if (hours < 10) {
      hours = "0" + hours.toString();
    } else {
      hours = hours.toString();
    }
    const timestamp =
      today.getFullYear().toString() +
      "-" +
      month.toString() +
      "-" +
      today.getDate().toString() +
      " " +
      hours +
      ":" +
      minutes +
      ":" +
      today.getSeconds().toString();
    event.preventDefault();
    setMessages([
      ...messages,
      { message: newMessage, username: myUsername, timestamp: timestamp },
    ]);
    dispatch(
      actions.sendMessage({
        message: newMessage,
        username: myUsername,
        chatID: focusedChat,
        timestamp: timestamp,
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

  const [chatName, setChatName] = useState("");
  /* HANDLING THE DISPLAY OF NEW MESSAGES AND NEW FOCUSED CHAT */
  React.useEffect(() => {
    if (listOfDms !== null && searching === false) {
      setMessages(rightChat(listOfDms));
      setChatName(getChatName(listOfDms));
      if (focusedChat) {
        dispatch(actions.resetLastSeen({ chatID: focusedChat }));
      }
    }
  }, [focusedChat, listOfDms, searching]);

  return (
    <div className="flex flex-col content-center focused-view-custom-bg">
      <div className="text-3xl text-white self-center focused-view-custom-bg">{chatName}</div>
      {focusedChat ? (
        <SearchBar
          id="search-chat"
          placeHolder="Search in this chat..."
          value={searchTerm}
          onButtonClick={handleSearchSubmit}
          onInputChange={handleSearchInput}
        />
      ) : null}
      {searching ? (
        <div
          className="text-white p-2 input-box-custom-bg self-center cursor-pointer mt-4"
          onClick={(e) => {
            setSearching(false);
          }}
        >
          Cancel Search
        </div>
      ) : null}
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
