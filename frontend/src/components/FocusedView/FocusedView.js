import React from "react";
import "../../assets/main.css";
import ChatContainer from "../ChatContainer/ChatContainer";
import ThreadContainer from "../ThreadContainer/ThreadContainer";

/**
 * FocusedView is the component that makes sure to route to specific component that is focused on
 * @property {String} focusedChat - a string used to check what chat we are focusing on
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * @property {String} focusedThread - a string used to check what thread we are focusing on 
 * @property {Function} handleFocusedThread - callback funktion for setting the focused thread
 * @property {Function} resetFocusedThread - callback for resetting the focused thread
 * @property {boolean} threadLock - used for conditional rendering of threads
 * @returns the appropriate component that should be displayed
 */
const FocusedView = ({ focusedChat, focusedPage, focusedThread, handleFocusedThread, resetFocusedThread, threadLock }) => {
  return (
    <>
      {focusedPage === "Home" ? (
        <ChatContainer focusedChat={focusedChat} />
      ) : (

        <ThreadContainer threadLock={threadLock} focusedThread={focusedThread} handleFocusedThread={handleFocusedThread} resetFocusedThread={resetFocusedThread} />
      )}
    </>
  );
};

export default FocusedView;
