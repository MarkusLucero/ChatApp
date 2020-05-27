import React from "react";
import comments from "../../img/comments.svg";
import startChat from "../../img/startChat.svg";
import { useSelector, useDispatch } from "react-redux";
import * as actions from "../../actions/actions.js";
import ReactTooltip from "react-tooltip";

/**
 * Threadinfo shows initial information about a thread
 * @property {object} thread the current thread object
 * @property {Function} handleFocusedThread - callback funktion for setting which thread we are focusing on
 * @returns a div with information about thread
 */
const ThreadInfo = ({ thread, handleFocusedThread }) => {
  const username = useSelector((state) => state.socketState.username);
  const dispatch = useDispatch();

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
        {thread.username !== username ? (
          <img
            data-tip
            data-for="createChatOP"
            className="self-end ml-auto cursor-pointer"
            src={startChat}
            onClick={() => {
              dispatch(
                actions.startChat({
                  chatName: thread.username,
                  from: username,
                  members: [username, thread.username],
                })
              );
            }}
            alt="add chat button"
          />
        ) : (
          ""
        )}
        <ReactTooltip id="createChatOP" place="left" effect="solid">
          Create a chat with OP!
        </ReactTooltip>
        <img
          id={thread.id}
          src={comments}
          className={
            thread.username !== username ? "self-end" : "self-end ml-auto"
          }
          alt="displaying num of comments"
        />
        <span className="self-end">{thread.comments.length}</span>
      </div>
    </div>
  );
};

export default ThreadInfo;
