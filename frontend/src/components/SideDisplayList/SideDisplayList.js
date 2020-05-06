import React, { useState } from "react";
import Servers from "./Servers";

/**
 * SideDisplayList holds all components in the side display - servers, add server, profile
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @returns A div containing the servers, add server, logout and profile buttons
 */
const SideDisplayList = ({handleFocusedPage}) => {

  // hardcoded server called GEN -- only one for now.
  const servers = [{name: "GEN"}];

  return (
    <div style={{backgroundColor: '#23272A' }} className="flex flex-col">
      <div className="border-solid border-b-2 border-gray-800 h-20">
        <h1 id="Home" className="cursor-pointer rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg text-white hover:bg-gray-500 flex items-center justify-center" onClick={handleFocusedPage}>
          HOME
        </h1>
      </div>
      <div className=" h-auto">
        <div className="flex flex-col justify-between">
          {servers.map((server, index) => (
            <Servers key={index} server={server} handleFocusedPage={handleFocusedPage} />
          ))}
        </div>
      </div>
    </div>
  );
};

export default SideDisplayList;