import React, { useState } from "react";
import * as actions from "../../../actions/actions";
import { useDispatch } from "react-redux";

/**
 * StartChat is the component which is seen when pressing the '+' icon next to active chats
 * @property {array} friends list containing the friends of the suer
 * @property {string} username the username of the user
 * @property {function} setShowAddChat callback function is used to show/hide the StartChat modal
 * @returns The start chat modal 
 */
const StartChat = ({ friends, username, setShowAddChat }) => {
  const dispatch = useDispatch();

  /*local state for which friend to start chat with */
  const [chosen, setChosen] = useState([username]);
  /* Used when clicking on a checkbox */
  const handleCheck = (e) => {
    e.persist();
    if (e.target.checked) {
      setChosen((prevFriends) => [...prevFriends, e.target.value]);
    } else {
      /*Friend was clicked, this removes that friend*/
      const prev = chosen.filter((friend) => friend !== e.target.value);
      setChosen(prev);
    }
  };

  /* handles the chatname */
  const [chatName, setChatName] = useState("");
  const handleChatName = (event) => {
    setChatName(event.target.value);
  };

  const startChat = () => {
    //only username in state, noone else chosen
    if (chosen.length === 1) {
      //TODO Display errors for the user???
      setShowAddChat(false);
    } else {
      const data = { chatName: chatName, from: username, members: chosen };
      dispatch(actions.startChat(data));
      setShowAddChat(false);
    }
  };

  return (
    <div
      style={{ left: "24rem", top: "16rem" }}
      className=" w-56 z-40 rounded mt-2 bg-gray-700 absolute p-2  w-auto"
    >
      <div className="text-white border-solid border-b border-black mb-2 pr-3">
        Choose friends to add:
      </div>
      {friends.map((friend, index) => {
        return (
          <div key={index} className="flex text-white flex-row justify-between">
            <div className="">{friend}</div>
            <input
              onChange={handleCheck}
              type="checkbox"
              className=" "
              value={friend}
            />
          </div>
        );
      })}
      <input
        className=" mt-2 text-white input-box-custom-bg shadow appearance-none "
        type="text"
        onChange={handleChatName}
        value={chatName}
        placeholder="Chat name..."
      />
      <button className="rounded w-full bg-blue-600 mt-4 cursor-pointer" onClick={startChat}>
        Create chat
      </button>
    </div>
  );
};

export default StartChat;
