import React from "react";
import "../../assets/main.css";
import ChatContainer from "../ChatContainer/ChatContainer";

/**
 * FocusedView is the component that makes sure to route to specific component that is focused on
 * TODO Actual routing
 * @returns the appropriate component that should be displayed
 */
const FocusedView = ({focusedChat}) => {
  return (
    <>
      <ChatContainer focusedChat = {focusedChat} />
    </>
  );
};

export default FocusedView;
