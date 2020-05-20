import React, { useState } from "react";
import Logout from "./Logout/Logout.js";
import Server from "./Server";

/**
 * SideDisplayList holds all components in the side display - servers, add server, profile
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @property {object} server the global server object
 * @property {function} resetFocusedThread callback for resetting the focused thread
 * @property {function} setThreadLock callback used in server for locking the threads
 * @returns A div containing the global server, Home and logout button
 */

const SideDisplayList = ({
  handleFocusedPage,
  server,
  resetFocusedThread,
  setThreadLock,
}) => {
  return (
    <div style={{ backgroundColor: "#23272A" }}>
      <div className="flex flex-col h-full">
        <div className="border-solid border-b-2 border-gray-800 h-20">
          <h1
            id="Home"
            className="cursor-pointer rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg text-white hover:bg-gray-500 flex items-center justify-center"
            onClick={handleFocusedPage}
          >
            HOME
          </h1>
        </div>
        <div className=" h-auto">
          <div className="flex flex-col justify-between">
            <Server
              server={server}
              handleFocusedPage={handleFocusedPage}
              resetFocusedThread={resetFocusedThread}
              setThreadLock={setThreadLock}
            />
          </div>
        </div>
      <div className=" mt-auto">
        <Logout />
      </div>
      </div>
    </div>
  );
};

export default SideDisplayList;
