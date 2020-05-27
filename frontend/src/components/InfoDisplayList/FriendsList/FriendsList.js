import React, { useState } from "react";
import { useDispatch, useSelector } from "react-redux";
import add_friend from "../../../img/add_friend.svg";
import AddFriend from "./AddFriend";
import * as actions from "../../../actions/actions";
import ReactTooltip from "react-tooltip";
/**
 * FriendsList contains a list of the users friends
 * @param {string} myUsername The currently logged in used
 * @param {function} setFocusedChat callback for focusing on a chat
 * @returns a div containing all friends for the logged in user
 */

const FriendsList = ({ myUsername, setFocusedChat}) => {
  const dispatch = useDispatch();

  const currentFriends = useSelector(
    (state) => state.socketState.listOfFriends
  );
  

  const currentChats = useSelector(
    (state) => state.socketState.listOfDms
  ); 

  const [allChats, setAllChats] = useState([]);
  const [friends, setFriends] = useState([]);

  /* state to toggle modal, state should preferably probably be handled
   inside modal for scalability and safety. Also refactor to use createPortal when as it is recommended.. */
  const [show, setShow] = useState(false);
  /* State that contains the username of the user we're trying to add */
  const [username, setUsername] = useState("");
  /* state controlling if the friend request was successful or not, TODO: addSuccessful must be set to false and username false once friend is added */
  const [addSuccessful, setAddSuccessful] = useState(false);
  /*Used to toggle hover effect*/
  const [hovered, setHovered] = useState(false);
  /* Gets the username of the user doing the friend request */
  const requester = useSelector((state) => state.socketState.username);
  /**
   * set the username of the user we want to add
   * @param event the event of the window object
   */
  const handleInputChange = (event) => {
    setUsername(event.target.value);
  };

  /* when addSuccesful changes and username is not null, refire and add a friend into the array of friends */
  React.useEffect(() => {
    if (addSuccessful && username) {
      dispatch(actions.addFriend({ username }));
      setUsername("");
      setAddSuccessful(false);
    }
  }, [addSuccessful]);

  const [newChat, setNewChat] = useState(false);

  React.useEffect(()=>{
    if(currentChats !=null && currentChats !==[]){
      let difference = currentChats.filter(x => !allChats.includes(x));
      setAllChats(currentChats);
      if(newChat && difference.length !== 0 && allChats !== []){
        setFocusedChat(difference[0].chatID);
        setNewChat(false);
      }
    }
  }, [currentChats]);
  /* when currentFriends changes - refire useEffect and add the new friends to friends state */
  React.useEffect(() => {
    if (currentFriends != null) {
      setFriends(currentFriends);
    }
  }, [currentFriends]);

  const startChat = (e) => {
    //cant start a chat if there's allready one with that person 
    if(!allChats.map(chat => chat.chatName === e.target.id).includes(true)){
      console.log("creating new chat");
      const data = { chatName: e.target.id , from: myUsername, members: [myUsername, e.target.id] };
      dispatch(actions.startChat(data));
      setNewChat(true);
    }
  };
  return (
    <div className="flex flex-col ml-2">
      <div
        id="friends"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Friends
        <img
          data-tip
          data-for="addFriendTip"
          src={add_friend}
          alt=""
          className={
            hovered
              ? "h-6 w-6 plusIcon-custom-hover cursor-pointer"
              : "h-6 w-6 cursor-pointer"
          }
          onClick={() => setShow(true)}
          onMouseEnter={() => {
            setHovered(!hovered);
          }}
          onMouseLeave={() => {
            setHovered(!hovered);
          }}
        />
        <ReactTooltip id="addFriendTip" place="right" effect="solid">
          Add a new friend!
        </ReactTooltip>
        {show ? (
          <div className="addFriend-custom-overlay">
            <AddFriend
              userInput={username}
              requester={requester}
              handleInputChange={handleInputChange}
              setAddSuccessful={setAddSuccessful}
              currentFriends={currentFriends}
              show={show}
              setShow={setShow}
              from={requester}
            ></AddFriend>
          </div>
        ) : null}
      </div>
      <div className="flex flex-col">
        {friends.map((friend, index) => {
          return (
            <div
              className="text-white text-xl hover:bg-gray-500 cursor-pointer"
              key={index}
              onClick ={(e)=>{startChat(e)}}
              id={friend}
            >
              {friend}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default FriendsList;
