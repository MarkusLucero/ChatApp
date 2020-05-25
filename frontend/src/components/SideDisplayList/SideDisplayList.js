import React from "react";
import Logout from "./Logout/Logout.js";
import Server from "./Server";
import homeButton from "../../img/home.svg";
import homeButtonHover from "../../img/homeHover.svg";
import ReactTooltip from "react-tooltip";

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
  const [hovered, setHovered] = React.useState(false);
  return (
    <div style={{ backgroundColor: "#23272A" }}>
      <div className="flex flex-col h-full">
        <div className="border-solid border-b-2 border-gray-800 h-20 flex flex-row justify-center">
          <ReactTooltip
            id="goToHome"
            place="right"
            effect="solid"
            backgroundColor="black"
          >
            Home Screen
          </ReactTooltip>
          <img
            data-tip
            data-for="goToHome"
            onMouseEnter={() => {
              setHovered(!hovered);
            }}
            onMouseLeave={() => {
              setHovered(!hovered);
            }}
            id="Home"
            src={hovered ? homeButtonHover : homeButton}
            className="cursor-pointer h-12 w-12 pt-2 mt-2 text-center"
            onClick={handleFocusedPage}
            alt="home button"
          />
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
