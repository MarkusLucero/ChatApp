import React from "react";
import * as actions from "../../actions/actions";
import { useDispatch, useSelector } from "react-redux";
import ReactTooltip from "react-tooltip";
import globalButton from "../../img/global.svg";
import globalButtonHover from "../../img/globalHover.svg";

const axios = require("axios");

/**
 * Component for displaying the global server
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @property {object} server the global server object
 * @property {Function} resetFocusedThread - callback for resetting the currently focused thread
 * @property {function} setThreadLock used for locking the rendering of threads
 * @returns the button of the global server
 */
const Server = ({
  server,
  handleFocusedPage,
  resetFocusedThread,
  setThreadLock,
}) => {
  /* useDispatch from dispatch function from store */
  const dispatch = useDispatch();

  /*Get the username, magictoken and list of threads from store */
  const username = useSelector((state) => state.socketState.username);
  const magic_token = useSelector((state) => state.socketState.magicToken);

  /*Reset the focused thread, so we show all threads in focusedview */
  const handle = (e) => {
    fetchThreads();
    resetFocusedThread();
    handleFocusedPage(e);
  };

  async function fetchThreads() {
    setThreadLock(false);
    try {
      const response = await axios.post(
        "/",
        JSON.stringify({
          action: "fetch_server_contents",
          server_name: server.serverName,
          magic_token: magic_token,
          username: username,
        })
      );
      const data = await response;
      switch (data.status) {
        case 200:
          {
            console.log(data.data);
            /*The threads in the list of threads recieced looks different, need to create new thredobjects from that information */
            let threads = [];
            for (const thread of data.data.threads) {
              threads.push({
                rootPost: {
                  rootHeader: thread.header,
                  rootComment: thread.text,
                },
                username: thread.creator,
                timestamp: thread.timestamp,
                comments: thread.comment_list,
                id: thread.thread_id,
              });
            }
            dispatch(actions.addThreads({ threads: threads }));
          }
          setThreadLock(true);
          break;
        case 404: {
          console.log(data);
          setThreadLock(true);
          break;
        }
        default:
          setThreadLock(true);
          alert("cannot fetch threads from server");
          break;
      }
    } catch (error) {
      console.log(error);
      setThreadLock(true);
    }
  }
  const [hovered, setHovered] = React.useState(false);
  return (
    <div className="flex flex-row justify-center">
      <img
        id={server.serverName}
        onMouseEnter={() => {
          setHovered(!hovered);
        }}
        onMouseLeave={() => {
          setHovered(!hovered);
        }}
        data-tip
        data-for="goToGlobalServer"
        src={hovered ? globalButtonHover : globalButton}
        onClick={handle}
        className="cursor-pointer h-12 w-12 pt-2 mt-2 text-center"
        alt="global server button"
      />
      <ReactTooltip
        id="goToGlobalServer"
        place="right"
        effect="solid"
        backgroundColor="black"
      >
        The Global Server
      </ReactTooltip>
    </div>
  );
};

export default Server;
