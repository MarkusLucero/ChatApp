import React, { useRef, useEffect } from "react";
import plus from "../../img/plus.svg";
import CreateThread from "./CreateThread";

/**
 * ThreadList is the component which lists all threads the user has access to
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * @property {string} username  username of the logged in user
 * @returns A div containing all threads that the logged in user can access
 */
const ThreadList = ({ focusedPage, username }) => {
 
 
  /* used for the hover effect on + sign */
  const [hovered, setHovered] = React.useState(false);
  const [showCreateThread, setShowCreateThread] = React.useState(false);
  return (
    <div className="flex flex-col ml-2">
      <div
        id="threadListHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700  mt-4 flex flex-row justify-between"
      >
        Your Threads
        <img
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
      </div>
      <div>
        {showCreateThread ? (
          <div className="addFriend-custom-overlay">
            <div className="w-1/2">
              <CreateThread showCreateThread ={showCreateThread} setShowCreateThread={setShowCreateThread} />
            </div>
          </div>
        ) : null}
      </div>
      <div id="threadList" className="text-white pt-1">
        list of threads created by you
        {/* MAP a list of thread components here */}
      </div>
    </div>
  );
};

export default ThreadList;
