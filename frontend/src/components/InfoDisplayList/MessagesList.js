import React, { useState } from "react";
import add_chat from "../../img/add_chat.png";
import StartChat from "./StartChat";

/**
 * contains the list of available direct messages that we can chat in
 *
 * @param {function} callback function used to get the target of what we are clicking on
 * returns a div containing all direct messages
 */
const MessagesList = ({ handleFocusedChat, username }) => {
  /* TODO actualy use the chat names from redux store */
  const [chats, setChats] = useState(["Skooben", "Grabbarna Grus"]);
  /* Determens if we're showing the add chat modal */
  const [showAddChat, setShowAddChat] = useState(false);

  return (
    <div className="flex flex-col ml-2">
      <div
        id="messagesList"
        className="mt-5 text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between">
        Direct Messages
        <img onClick ={() => {setShowAddChat(true)}} src={add_chat} alt="" className="h-6 w-6"/>
      </div>
      {showAddChat ? <StartChat username={username} friends ={chats}/> : null}
      <div className="flex flex-col">
        {chats.map((chat, index) => {
          return (
            <div
              className="text-white text-xl hover:bg-gray-500"
              onClick={handleFocusedChat}
              key={index}
            >
              {chat}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default MessagesList;
