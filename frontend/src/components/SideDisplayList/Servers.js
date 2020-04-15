import React from 'react';


const Servers = ({ server }) => {
    return (
        <div className="rounded-full h-16 w-16 mb-2 mt-2 bg-gray-600 hover:bg-gray-500 flex items-center justify-center"> 
            {server.name}
        </div>
    )
}

export default Servers;

