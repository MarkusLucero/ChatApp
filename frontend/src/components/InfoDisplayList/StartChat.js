import React, {useState} from 'react';
import * as actions from "../../actions/actions";
import { useDispatch } from "react-redux";

const StartChat = ({friends, username}) => {
      /* useDispatch from dispatch function from store */
    const dispatch = useDispatch();

    /*local state for which friend to start chat with     */
    const [chosen, setChosen] = useState([]);

    const handleCheck = (e) =>{
        e.persist();
        if(e.target.checked)
        {
            setChosen(prevFriends => [...prevFriends, e.target.value]); 
        }
        else
        {
            const prev = chosen.filter(friend => friend != e.target.value); 
            setChosen(prev);
        }
    };

    const startChat= () => {
        const data = {chat_name: username, from: username, members: chosen};
        dispatch(actions.startChat(data));
    };

    return (
        <div style={{left: "24rem"}}className=" w-56 z-40 bg-white rounded mt-2 bg-gray-700 absolute p-2" >
            <div className="border-solid border-b border-black">
            Choose friends to add:
            </div>
            {friends.map((friend, index) => {

                return <div key={index} className="flex flex-row justify-between">
                             <div className="">{friend}</div>
                             <input onChange ={handleCheck} type="checkbox" value ={friend}/>
                        </div>
            })} 
            <button className="rounded w-full bg-blue-600 mt-4" onClick={startChat}>Create chat</button>
        </div>
    )
}

export default StartChat;
