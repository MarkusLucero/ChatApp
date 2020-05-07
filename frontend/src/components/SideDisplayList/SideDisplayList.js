import React, { useState } from "react";
import Logout from "./Logout/Logout.js"
import Servers from "./Servers";

/**
 * SideDisplayList holds all components in the side display - servers, add server, profile
 * @param  props - The props passed down from LandingPage - servers and additional information
 * @returns A div containing the servers, add server and profile
 */
const SideDisplayList = (props) => {
  /* The initial state, all the servers -- WILL MOST LIKELY BE PASSED DOWN AS PROPS INSTEAD */
  const [servers, setServers] = useState([{ name: "OSPP" }, { name: "OSPP" }]);

  return (
    <div style={{backgroundColor: '#23272A' }} className="flex flex-col">
      <div className="border-solid border-b-2 border-gray-700 h-20">
        <h1 className="rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 flex items-center justify-center">
          HOME
        </h1>
      </div>
      <div className="border-solid border-b-2 border-gray-700 h-auto">
        <div className="flex flex-col justify-between">
          {servers.map((server, index) => (
            <Servers key={index} server={server} />
          ))}
        </div>
      </div>
      <div className="border-solid border-b-2 border-gray-700">
        <div className="rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 flex items-center justify-center ">
          <h1 className="text-6xl flex items-center justify-center pb-3">+</h1>
        </div>
      </div>
      <Logout/>
    </div>
  );
};

export default SideDisplayList;