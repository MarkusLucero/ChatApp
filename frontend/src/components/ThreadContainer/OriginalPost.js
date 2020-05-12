import React, { useEffect, useState } from "react";
import { useSelector } from "react-redux";
import CommentContainer from "../CommentContainer/CommentContainer";

/**
 * OriginalPost is the first post of a Thread
 * @property {string} focusedThread the name of the thread that we are currently focusing on
 * @returns a div containing all the information regarding the original post
 */
const OriginalPost = ({ focusedThread }) => {
  // list of all threads in server object from redux store
  const listOfThreads = useSelector(
    (state) => state.socketState.server.listOfThreads
  );

  //helper function for retrieving the right thread from state
  const rightThread = (list) => {
    for (const thread of list) {
      if (thread.id === focusedThread) {
        console.log(thread);
        return thread;
      }
    }
    return [];
  };

  /*local state for actual thread object*/
  const [thread, setThread] = useState(rightThread(listOfThreads));
  /* state for thread comments */

  /*update the focused thread when clicked, refire when focusedThread or state's thread is updated */
  useEffect(() => {
    console.log(thread);
    if (listOfThreads != null) {
      const actual = rightThread(listOfThreads);
      setThread(actual);
    }
  }, [focusedThread, listOfThreads]);

  return (
    <div>
      <div className="rounded border-2 border-solid border-black w-full p-2 mt-2 flex flex-col shadow-inner shadow-lg bg-gray-700">
        <div className="flex flex-row justify-between">
          <div className=" text-3xl text-white pl-2 w-auto">
            {thread.rootPost.rootHeader}
          </div>
          <div className="text-xs text-white">
            Posted by: {thread.username} on {thread.timestamp}
          </div>
        </div>
        <div className="text-xl text-white ml-5 ">
          {thread.rootPost.rootComment}
        </div>
      </div>
      <CommentContainer thread={thread} focusedThread={focusedThread}></CommentContainer>
    </div>
  );
};

export default OriginalPost;
