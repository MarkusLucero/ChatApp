import React, {useState} from 'react';
import placeholder from "../../img/placeholder.png";
import add_friend from "../../img/add_friend.webp";
/**
 * FriendsList holds the logged in members name and his friends
 * @property username of the logged in user
 * @returns a div containing all friends for the logged in user
 */
const FriendsList = ({username}) => {

    const [friends, setFriends] = useState(["Skooben", "Travie", "Mustafa"]);  //TODO: FIXA VÄNNER
    const [chats, setChats] = useState(["Skobah", "Donald Trump"]); 

    return (
        <div className="flex flex-col ml-2">
            <div id="profile"className="h-20 text-4xl flex flex-auto mr-4 text-white  mb-10"> 
                <img className ="rounded-full w-16 h-auto mr-5" alt="" src ={placeholder}/> 
                {username}
            </div>
            <div id="friends" className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between" >
                Friends
                <img src={add_friend} alt="" className="h-6 w-6"/> {/*TODO ON CLICK, lägg till bild  */}

            </div>
            <div className="flex flex-col">
                {friends.map((friend, index) => {
                    return <div className="text-white text-xl hover:bg-gray-500"key={index}> {friend}</div>
                })}

            </div>
            
            <div id="chats" className="mt-5 text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-col">
                Chats
            </div>
            <div className="flex flex-col">
                {chats.map((chat, index) =>{
                    return <div className="text-white text-xl hover:bg-gray-500" key={index}>{chat}</div>
                })}
            </div>

        </div>
    )
}



export default FriendsList;
