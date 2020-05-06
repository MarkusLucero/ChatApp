import React from 'react';


const Server = ({ server, handleFocusedPage }) => {
    return (
        <div id={server.serverName} onClick={handleFocusedPage} className="rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 flex items-center justify-center"> 
            {server.serverName}
        </div>
    )
}

export default Server;