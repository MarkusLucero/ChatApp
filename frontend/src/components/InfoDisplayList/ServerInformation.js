import React from "react";
import { useSelector } from "react-redux";

const ServerInformation = ({ focusedPage }) => {

  const serverDescription = useSelector(state => state.socketState.server.serverInformation)

  return (
    <div className="flex flex-col ml-2">
      <div
        id="serverInformationHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Beskrivning
      </div>
      <div id="serverInformation" className="text-white">
        {serverDescription}
      </div>
    </div>
  );
};

export default ServerInformation;
