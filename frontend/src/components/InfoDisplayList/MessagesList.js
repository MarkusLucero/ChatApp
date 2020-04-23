import React, { useState } from "react";

/**
 * contains the list of available direct messages that we can chat in
 *
 * @param {function} callback function used to get the target of what we are clicking on
 * returns a div containing all direct messages
 */
const MessagesList = ({ handleFocusedChat }) => {
  /* TODO actualy use the chat names from redux store */
  const [chats, setChats] = useState(["Skooben", "Grabbarna Grus"]);

  return (
    <div className="flex flex-col ml-2">
      <div
        id="messagesList"
        className="mt-5 text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-col"
      >
        Direct Messages
      </div>
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
