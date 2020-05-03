import React from 'react';


const Servers = ({ server, handleFocusedPage }) => {
    return (
        <div id={server.name} onClick={handleFocusedPage} className="cursor-pointer rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 text-white flex items-center justify-center"> 
            {server.name}
        </div>
    )
}

export default Servers;