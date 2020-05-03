import React from "react";

const ServerInformation = ({ focusedPage }) => {
  return (
    <div className="flex flex-col ml-2">
      <div
        id="serverInformationHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Beskrivning
      </div>
      <div id="serverInformation" className="text-white">
        HÄR FINNS DET INFORMATION OM SERVERN
      </div>
    </div>
  );
};

export default ServerInformation;
