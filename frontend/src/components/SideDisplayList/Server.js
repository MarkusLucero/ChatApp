import React from 'react';
import * as actions from "../../actions/actions";
import { useDispatch, useSelector } from "react-redux";

const axios = require("axios");

/**
 * Component for displaying the global server
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @property {object} server the global server object
 * @property {Function} resetFocusedThread - callback for resetting the currently focused thread
 * @returns the button of the global server
 */
const Server = ({ server, handleFocusedPage, resetFocusedThread }) => {

    /* useDispatch from dispatch function from store */
    const dispatch = useDispatch();

    /*Get the username, magictoken and list of threads from store */
    const username = useSelector((state) => state.socketState.username);
    const magic_token = useSelector((state) => state.socketState.magicToken);
  
    /*Reset the focused thread, so we show all threads in focusedview */
    const handle = (e)=>{
        fetchThreads();
        resetFocusedThread();
        handleFocusedPage(e);
    }

    async function fetchThreads() {
        try {
            const response = await axios.post(
              "/",
              JSON.stringify({
                action: "fetch_server_contents",
                server_name: server.serverName,
                magic_token: magic_token ,
                username: username
              })
            );
            const data = await response;
            switch (data.status) {
              case 200: {
                console.log(data.data);

                /*The threads in the list of threads recieced looks different, need to create new thredobjects from that information */
                let threads = [];
                for (const thread of data.data.threads){
                    threads.push({rootPost: {rootHeader: thread.header, rootComment: thread.text}, username: thread.creator, timestamp: thread.timestamp, comments: thread.comment_list, id: thread.thread_id });
                };
                dispatch(actions.addThreads(threads));
                }   
                break;
              case 404: {
                console.log(data);
                break;
              }
              default:
                alert("cannot fetch threads from server");
                break;
            }
          } catch (error) {
            console.log(error);
          }
    };

    return (
        <div id={server.serverName} onClick={handle} className="rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 flex items-center justify-center"> 
            {server.serverName}
        </div>
    )
}

export default Server;