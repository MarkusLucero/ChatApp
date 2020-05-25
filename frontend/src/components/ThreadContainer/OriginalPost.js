import React from "react";

/**
 * OriginalPost is the first post of a Thread
 * @property {string} focusedThread the name of the thread that we are currently focusing on
 * @property {function} resetFocusedThread callback for resetting which thread we are focusing on
 * @returns a div containing all the information regarding the original post
 */
const OriginalPost = ({ thread, resetFocusedThread }) => {
  return (
    <div
      onClick={resetFocusedThread}
      style={{ backgroundColor: "#2C2F33" }}
      className="w-full p-2 mt-3 flex flex-col shadow-inner shadow-lg"
    >
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
  );
};

export default OriginalPost;
