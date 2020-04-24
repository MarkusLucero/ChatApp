import React, { useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import add_friend from "../../img/add_friend.webp";
import AddFriend from "./AddFriend";
import * as actions from "../../actions/actions";

/**
 * FriendsList holds the logged in members name and his friends
 * @property username of the logged in user
 * @returns a div containing all friends for the logged in user
 */

const FriendsList = () => {
  /* Get friendslist from redux store */
  const currentFriends = useSelector(
    (state) => state.socketState.listOfFriends
  );

  const [friends, setFriends] = useState([]);

  /* state to toggle modal, state should preferably probably be handled
   inside modal for scalability and safety. Also refactor to use createPortal when as it is recommended.. */
  const [show, setShow] = useState(false);
  const [username, setUsername] = useState("");
  const dispatch = useDispatch();
  const requester = useSelector((state) => state.socketState.username);

  /* Sends a websocket request to add the specific user */
  const handleAddFriend = (event) => {
    event.preventDefault();
    dispatch(actions.addFriend({ user_id: username, from: requester }));
    setUsername("");
  };

  const handleInputChange = (event) => {
    setUsername(event.target.value);
  };

  /* when currentFriends changes - refire useEffect and add the new friends to friends state */
  React.useEffect(() => {
    console.log("potato")
    if (currentFriends != null) {
      setFriends(currentFriends);
    }
  }, [currentFriends]);

  return (
    <div className="flex flex-col ml-2">
      <div
        id="friends"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Friends
        <img
          src={add_friend}
          alt=""
          className="h-6 w-6"
          onClick={() => setShow(true)}
        />{" "}
        {/*TODO ON CLICK, lägg till bild  */}
        <AddFriend
          userInput={username}
          handleInputChange={handleInputChange}
          handleAddFriend={handleAddFriend}
          show={show}
          setShow={setShow}
        ></AddFriend>
      </div>
      <div className="flex flex-col">
        {friends.map((friend, index) => {
          return (
            <div className="text-white text-xl hover:bg-gray-500" key={index}>
              {" "}
              {friend}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default FriendsList;
