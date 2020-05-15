import React from 'react';

/**
 * Component for displaying the global server
 * @property {function} handleFocusedPage Callback function passed down from LandingPage - Used to get id of what page we click on
 * @property {object} server the global server object
 * @property {Function} resetFocusedThread - callback for resetting the currently focused thread
 * @returns the button of the global server
 */
const Server = ({ server, handleFocusedPage, resetFocusedThread }) => {
    /*Reset the focused thread, so we show all threads in focusedview */
    const handle = (e)=>{
        resetFocusedThread();
        handleFocusedPage(e);
    }
    return (
        <div id={server.serverName} onClick={handle} className="rounded-full h-16 w-16 mb-2 mt-2 input-box-custom-bg hover:bg-gray-500 flex items-center justify-center"> 
            {server.serverName}
        </div>
    )
}

export default Server;