import React from "react";
import "../../assets/main.css";
import ChatContainer from "../ChatContainer/ChatContainer";
import ThreadContainer from "../ThreadContainer/ThreadContainer";

/**
 * FocusedView is the component that makes sure to route to specific component that is focused on
 * @property {String} focusedChat - a string used to check what chat we are focusing on
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * @returns the appropriate component that should be displayed
 */
const FocusedView = ({ focusedChat, focusedPage }) => {
  return (
    <>
      {focusedPage === "Home" ? (
        <ChatContainer focusedChat={focusedChat} />
      ) : (
        <ThreadContainer focusedPage={focusedPage} />
      )}
    </>
  );
};

export default FocusedView;
