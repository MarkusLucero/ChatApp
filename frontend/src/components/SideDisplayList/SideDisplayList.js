import React, { useState } from "react";
import Logout from "./Logout/Logout.js"
import Server from "./Server";

/**
 * SideDisplayList holds all components in the side display - servers, add server, profile
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @returns A div containing the servers, add server, logout and profile buttons
 */

const SideDisplayList = ({handleFocusedPage, server}) => {

  return (
    <div style={{backgroundColor: '#23272A' }} className="flex flex-col">
      <div className="border-solid border-b-2 border-gray-800 h-20">
        <h1 id="Home" className="cursor-pointer rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg text-white hover:bg-gray-500 flex items-center justify-center" onClick={handleFocusedPage}>
          HOME
        </h1>
      </div>
      <div className=" h-auto">
        <div className="flex flex-col justify-between">
            <Server server={server} handleFocusedPage = {handleFocusedPage} />
        </div>
      </div>
      <Logout/>
    </div>
  );
};

export default SideDisplayList;