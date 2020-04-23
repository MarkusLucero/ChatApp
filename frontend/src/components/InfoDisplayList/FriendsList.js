import React, { useState } from "react";
import add_friend from "../../img/add_friend.webp";
/**
 * FriendsList holds the logged in members name and his friends
 * @property username of the logged in user
 * @returns a div containing all friends for the logged in user
 */
const FriendsList = () => {
  const [friends, setFriends] = useState(["Skooben", "Travie", "Mustafa"]); //TODO: FIXA VÄNNER

  return (
    <div className="flex flex-col ml-2">
      <div
        id="friends"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Friends
        <img src={add_friend} alt="" className="h-6 w-6" />{" "}
        {/*TODO ON CLICK, lägg till bild  */}
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
