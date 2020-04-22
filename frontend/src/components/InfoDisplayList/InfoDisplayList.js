import React  from 'react';
import { useDispatch, useSelector } from "react-redux";
import FriendsList from './FriendsList';


const InfoDisplayList = (props) => {

    /* get the logged in  username from redux store */
    const username = useSelector((state) => state.loginState.username);

    return (
        <div style={{backgroundColor: "#2C2F33"}} className="border-solid border-l-4 border-gray-700">
            {/*<FriendsList username = {username} />*/}
            
        </div>
    )
}


export default InfoDisplayList;
