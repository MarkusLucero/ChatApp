import React from "react";


/**
 * Get and display the correct information about the server of which we are focusing on
 * @param {String} focusedPage a string used to check what page we are focusing on
 * @returns a div contianing the description of the server that we are focusing on
 */
const ServerInformation = ({ focusedPage }) => {

  //const serverDescription = useSelector(state => state.socketState.server.serverInformation)

  return (
    <div className="flex flex-col ml-2">
      <div
        id="serverInformationHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700 flex flex-row justify-between"
      >
        Beskrivning
      </div>
      <div id="serverInformation" className="text-white">
        This is the global server that everyone joins. Make threads, comment and be happy peeps.
      </div>
    </div>
  );
};

export default ServerInformation;
