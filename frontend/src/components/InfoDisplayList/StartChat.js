import React, {useState} from 'react';
import * as actions from "../../actions/actions";
import { useDispatch } from "react-redux";

/**
 * StartChat is the component which is seen when pressing the '+' icon next to active chats
 * @property friends, all friends of the logged in user
 * @property username, the logged in user's username
 * @param {function} setShowAddChat callback is used to show/hide the StartChat modal
 * @returns A div where user can choose which friend he want's to chat with
 */
const StartChat = ({friends, username, setShowAddChat}) => {
      /* useDispatch from dispatch function from store */
    const dispatch = useDispatch();

    /*local state for which friend to start chat with     */
    const [chosen, setChosen] = useState([username]);
    /* Used when clicking on a checkbox */
    const handleCheck = (e) =>{
        e.persist();
        if(e.target.checked)
        {
            setChosen(prevFriends => [...prevFriends, e.target.value]); 
        }
        else
        {
            /*Friend was clicked, this removes that friend*/
            const prev = chosen.filter(friend => friend != e.target.value); 
            setChosen(prev);
        }
    };

    const startChat= () => {
        //only username in state, noone else chosen
        if(chosen.length == 1){
            setShowAddChat(false);
        }
        else{
            const data = {chat_name: username, from: username, members: chosen};
            dispatch(actions.startChat(data));
            setShowAddChat(false);
        }

    };
    
    return (
        <div style={{left: "24rem", top: "16rem"}} className=" w-56 z-40 rounded mt-2 bg-gray-700 absolute p-2  w-auto" >
            <div className="border-solid border-b border-black mb-2 pr-3" >
            Choose friends to add:
            </div>
            {friends.map((friend, index) => {

                return <div key={index} className="flex flex-row justify-between">
                             <div className="">{friend}</div>
                             <input onChange ={handleCheck} type="checkbox" className=" "value ={friend}/>
                        </div>
            })} 
            <button className="rounded w-full bg-blue-600 mt-4" onClick={startChat}>Create chat</button>
        </div>
    )
}

export default StartChat;
