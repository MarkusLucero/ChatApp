import React from "react";
import comments from "../../img/comments.svg";
/**
 * Threadinfo shows initial information about a thread
 * @property {object} thread the current thread object
 * @property {Function} handleFocusedThread - callback funktion for setting which thread we are focusing on
 * @returns a div with information about thread
 */
const ThreadInfo = ({ thread, handleFocusedThread }) => {
  return (
      <div
        id={thread.id}
        style={{ backgroundColor: "#2C2F33" }}
        onClick={handleFocusedThread}
        className="w-full p-2 mt-3 flex flex-col shadow-inner shadow-lg"
      >
        <div id={thread.id} className="flex flex-row">
          <div id={thread.id} className="border-2 w-20 h-20 "></div>
          <div id={thread.id}>
            <div id={thread.id} className="text-xs pl-2 text-white">
              Posted by: {thread.username} on {thread.timestamp}
            </div>
            <div id={thread.id} className=" text-3xl text-white pl-2 w-auto">
              {thread.rootPost.rootHeader}
            </div>
          </div>
          <img
            id={thread.id}
            src={comments}
            className="self-end ml-auto"
            alt="displaying num of comments"
          />
          <span className="self-end">{thread.comments.length}</span>
        </div>
      </div>
  );
};

export default ThreadInfo;
