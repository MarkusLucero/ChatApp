import React from "react";
import plus from "../../../img/plus.svg";
import CreateThread from "./CreateThread";
import { useSelector } from "react-redux";
import ReactTooltip from "react-tooltip";

/**
 * ThreadList is the component which lists all threads the user has access to
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * @property {string} username  username of the logged in user
 * @property {function} handleFocusedThread callback used when changing what thread to focus on
 * @returns A div containing all threads that the logged in user can access
 */
const ThreadList = ({ focusedPage, username, handleFocusedThread }) => {
  /* used for the hover effect on + sign */
  const [hovered, setHovered] = React.useState(false);

  const [showCreateThread, setShowCreateThread] = React.useState(false);

  /* used for mapping threads in the threadlist component so they are displayed*/
  const [threads, setThreads] = React.useState([]);

  // list of all threads in server object from redux store
  const listOfThreads = useSelector(
    (state) => state.socketState.server.listOfThreads
  );

  //if listOfThreads changes add it to threads state so it can be displayed
  React.useEffect(() => {
    if (listOfThreads != null) {
      setThreads(listOfThreads);
    }
  }, [listOfThreads]);
  /*filter all threads matching the currently logged in username*/
  const yourThreads = threads.filter((thread) => thread.username === username);

  return (
    <div className="flex flex-col ml-2">
      <div
        id="threadListHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700  mt-4 flex flex-row justify-between"
      >
        Threads created by you:
        <img
          data-tip
          data-for="createThreadTip"
          src={plus}
          onMouseEnter={() => {
            setHovered(!hovered);
          }}
          onMouseLeave={() => {
            setHovered(!hovered);
          }}
          alt="Create a new thread!"
          className={
            hovered
              ? "plusIcon-custom-hover h-6 w-6 cursor-pointer"
              : "h-6 w-6 cursor-pointer"
          }
          onClick={() => {
            setShowCreateThread(true);
          }}
        />
        <ReactTooltip id="createThreadTip" place="right" effect="solid">
          Create a new thread!
        </ReactTooltip>
      </div>
      <div>
        {showCreateThread ? (
          <div className="addFriend-custom-overlay">
            <div className="w-1/2">
              <CreateThread
                showCreateThread={showCreateThread}
                setShowCreateThread={setShowCreateThread}
              />
            </div>
          </div>
        ) : null}
      </div>
      <div
        id="threadList"
        className="text-white pt-1 h-screen25 overflow-y-scroll"
      >
        {yourThreads.map((thread, index) => {
          return (
            <div
              className="text-white text-xl hover:bg-gray-500 cursor-pointer"
              onClick={handleFocusedThread}
              id={thread.id}
              key={index}
            >
              {thread.rootPost.rootHeader}
            </div>
          );
        })}
      </div>
    </div>
  );
};

export default ThreadList;
