import React from 'react';

/**
 * Threadinfo shows initial information about a thread
 * @property {object} thread the current thread object
 * @property {Function} handleFocusedThread - callback funktion for setting which thread we are focusing on
 * @returns a div with information about thread
 */
const ThreadInfo = ({ thread, handleFocusedThread }) => {
    return (
        <div id ={thread.id} onClick ={handleFocusedThread} className="rounded border-2 border-solid border-black w-full p-2 mt-2 flex flex-col shadow-inner shadow-lg bg-gray-800 cursor-pointer">
        <div id ={thread.id} className="flex flex-row justify-between">
          <div id = {thread.id}className=" text-3xl text-white pl-2 w-auto">
            {thread.rootPost.rootHeader}
          </div>
          <div id ={thread.id} className="text-xs text-white">
            Posted by: {thread.username} on {thread.timestamp}
          </div>
        </div>

      </div>
    )
}

export default ThreadInfo;

