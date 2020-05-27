import React  from 'react';
import { useSelector } from "react-redux";
import InformationHeader from './InformationHeader/InformationHeader';
import FriendsList from './FriendsList/FriendsList';
import ServerInformation from './InformationHeader/ServerInformation';
import ThreadList from './ThreadList/ThreadList';
import MessagesList from './MessagesList/MessagesList';


/**
 * contains everything that needs to be displayed in the InfoDisplayList 
 *
 * @property {function} handleFocusedChat callback function used to get the target of what we are clicking on in MessagesList
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * @property {String} focusedChat - a string used to check what chat we are focusing on
 * @property {function} handleFocusedThread callback function used to get the target of what we are clicking on in ThreadList
 * returns a div containing the component InformationHeader, FriendsList and MessagesList
 */
const InfoDisplayList = ({handleFocusedChat, focusedPage, focusedChat, handleFocusedThread, setFocusedChat}) => {

    /* get the logged in  username from redux store */
    const username = useSelector((state) => state.socketState.username);

    return (
        <div style={{backgroundColor: "#2C2F33"}} className="p-1">
            <InformationHeader username = {username} focusedPage={focusedPage}/>
            {focusedPage === "Home" ? <FriendsList setFocusedChat={setFocusedChat} myUsername={username}/> : <ServerInformation focusedPage={focusedPage} />}
            {focusedPage === "Home" ? <MessagesList  username = {username} focusedChat={focusedChat} handleFocusedChat = {handleFocusedChat}/> : <ThreadList handleFocusedThread={handleFocusedThread} username = {username} focusedPage={focusedPage}/> }
        </div>
    )
}


export default InfoDisplayList;
