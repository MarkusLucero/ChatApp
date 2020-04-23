import React, { useState, useEffect, useRef } from "react";
import add_chat from "../../img/add_chat.png";
import StartChat from "./StartChat";

/**
 * contains the list of available direct messages that we can chat in
 *
 * @param {function} callback function used to get the target of what we are clicking on
 * @property the username of the logged in user
 * returns a div containing all direct messages
 */
const MessagesList = ({ handleFocusedChat, username }) => {
  /* TODO actualy use the chat names from redux store */
  const [chats, setChats] = useState(["Skooben", "Grabbarna Grus"]);
  /* Determens if we're showing the add chat modal */
  const [showAddChat, setShowAddChat] = useState(false);

  /*On mount, add event listerner for click outside ref addChatModal*/ 
  useEffect(()=> {
   document.addEventListener('mousedown', handleClick);
   return () =>{
     document.removeEventListener('mousedown', handleClick);
   }
  }, []);

  /*Handle click outside addChatModal */
  const handleClick = e => {
    if (addChatModal.current.contains(e.target)) {
      // inside click
      return;
    }
    else{
      setShowAddChat(false);
    }
  };
  /*Keep track of the modal */
  const addChatModal = useRef();



  return (
    <div className="flex flex-col ml-2">
      <div
        id="messagesList"
        className="mt-5 text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between">
        Direct Messages
        <img onClick ={() => {setShowAddChat(true)}} src={add_chat} alt="Create a new chat!" className="h-6 w-6"/>
      </div>
      <div ref ={addChatModal}>{showAddChat ? <StartChat setShowAddChat = {setShowAddChat} username={username} friends ={chats}/> : null}</div>
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
