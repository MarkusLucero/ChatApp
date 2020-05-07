import React  from 'react';
import { useDispatch, useSelector } from "react-redux";
import InformationHeader from './InformationHeader';
import FriendsList from './FriendsList';
import ServerInformation from './ServerInformation';
import ThreadList from './ThreadList';
import MessagesList from './MessagesList';


/**
 * contains everything that needs to be displayed in the InfoDisplayList 
 *
 * @property {function} handleFocusedChat callback function used to get the target of what we are clicking on in MessagesList
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * returns a div containing the component InformationHeader, FriendsList and MessagesList
 */
const InfoDisplayList = ({handleFocusedChat, focusedPage, handleFocusedThread}) => {

    /* get the logged in  username from redux store */
    const username = useSelector((state) => state.socketState.username);

    return (
        <div style={{backgroundColor: "#2C2F33"}} className="p-1">
            <InformationHeader username = {username} focusedPage={focusedPage}/>
            {focusedPage === "Home" ? <FriendsList /> : <ServerInformation focusedPage={focusedPage} />}
            {focusedPage === "Home" ? <MessagesList  username = {username} handleFocusedChat = {handleFocusedChat} /> : <ThreadList handleFocusedThread={handleFocusedThread} username = {username} focusedPage={focusedPage}/> }
        </div>
    )
}


export default InfoDisplayList;
