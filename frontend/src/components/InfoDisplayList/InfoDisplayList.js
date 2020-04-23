import React  from 'react';
import { useDispatch, useSelector } from "react-redux";
import InformationHeader from './InformationHeader';
import FriendsList from './FriendsList';
import MessagesList from './MessagesList';


/**
 * contains everything that needs to be displayed in the InfoDisplayList 
 *
 * @param {function} callback function used to get the target of what we are clicking on in MessagesList
 * returns a div containing the component InformationHeader, FriendsList and MessagesList
 */
const InfoDisplayList = ({handleFocusedChat}) => {

    /* get the logged in  username from redux store */
    const username = useSelector((state) => state.loginState.username);

    return (
        <div style={{backgroundColor: "#2C2F33"}} className="border-solid border-l-4 border-gray-700">
            <InformationHeader username = {username}/>
            <FriendsList />
            <MessagesList  username = {username} handleFocusedChat = {handleFocusedChat} />
        </div>
    )
}


export default InfoDisplayList;
