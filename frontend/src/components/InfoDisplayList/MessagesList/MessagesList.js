import React, { useState, useEffect, useRef } from "react";
import plus from "../../../img/plus.svg";
import StartChat from "./StartChat";
import { useSelector } from "react-redux";
import ReactTooltip from "react-tooltip";

/**
 * contains the list of available direct messages that we can chat in
 * @param {function} handleFocusedChat callback function used to get the target of what we are clicking on
 * @property {string} username  username of the logged in user
 * @property {String} focusedChat - a string used to check what chat we are focusing on
 * returns a div containing all direct messages
 */
const MessagesList = ({ handleFocusedChat, username, focusedChat }) => {
  /* Get friendslist from redux store */
  const friends = useSelector((state) => state.socketState.listOfFriends);

  /* Get list of dms from redux store */
  const chatObjects = useSelector((state) => state.socketState.listOfDms);

  const [chats, setChats] = useState([]);

  useEffect(() => {
    if (chatObjects !== null) {
      setChats(chatObjects);
    }
  }, [chatObjects]);

  /* Determens if we're showing the add chat modal */
  const [showAddChat, setShowAddChat] = useState(false);
  /* Used to highlight the add icon when hovered */
  const [hovered, setHovered] = useState(false);
  /*On mount, add event listerner for click outside ref addChatModal*/
  useEffect(() => {
    document.addEventListener("mousedown", handleClick);
    return () => {
      document.removeEventListener("mousedown", handleClick);
    };
  }, []);

  /**
   * handles a click outside of the create chat modal
   * @param event of the window object
   */
  const handleClick = (event) => {
    if (addChatModal.current.contains(event.target)) {
      // inside click
      return;
    } else {
      setShowAddChat(false);
    }
  };
  /*Keep track of the modal */
  const addChatModal = useRef();

  return (
    <div className="flex flex-col ml-2">
      <div
        id="messagesList"
        className="mt-5 text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Direct Messages
        <img
          data-tip
          data-for="createChatTip"
          onClick={() => {
            setShowAddChat(true);
          }}
          src={plus}
          onMouseEnter={() => {
            setHovered(!hovered);
          }}
          onMouseLeave={() => {
            setHovered(!hovered);
          }}
          alt="Create a new chat!"
          className={
            hovered
              ? "plusIcon-custom-hover h-6 w-6 cursor-pointer"
              : "h-6 w-6 cursor-pointer"
          }
        />
        <ReactTooltip id="createChatTip" place="right" effect="solid">
          Create a new chat!
        </ReactTooltip>
      </div>

      <div ref={addChatModal}>
        {showAddChat ? (
          <StartChat
            setShowAddChat={setShowAddChat}
            username={username}
            friends={friends}
          />
        ) : null}
      </div>
      <div className="flex flex-col">
        {chats.map((chat, index) => {
          return (
            <div
              onClick={handleFocusedChat}
              key={index}
              id={chat.chatID}
              className="text-white text-xl hover:bg-gray-500 cursor-pointer flex flex-row justify-between"
            >
              {/* TODO FIX SAME ID BAD PRAXIS */}
              <div id={chat.chatID}>{chat.chatName} </div>
              <div className="text-red-700">
                {focusedChat === chat.chatID
                  ? ""
                  : !chat.sinceLastSeen
                  ? ""
                  : "+" + chat.sinceLastSeen}
              </div>
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default MessagesList;
