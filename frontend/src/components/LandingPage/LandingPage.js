import React from "react";
import "../../assets/main.css";
import SideDisplayList from "../SideDisplayList/SideDisplayList";
import FocusedView from "../FocusedView/FocusedView";
import InfoDisplayList from "../InfoDisplayList/InfoDisplayList";
/**
 * LandingPage holds the layout design grid of the app. Also manages
 * the routing and display of everything inside SideDisplayList, InfoDisplayList and FocusedView
 *
 * @returns a div containing the SideDisplayList, InfoDisplayList and FocusedView components
 */
const LandingPage = () => {

  /* state to check what chat we are currently focusing on */
  const [focusedChat, setFocusedChat] = React.useState(null);

  /* callback function for getting the id of the direct message div that we are clicking on */
  const handleFocusedChat = (event) => {
    setFocusedChat(event.target.id);
  };

  return (
    /**
     *  The styling is made up of Tailwind css classes
     *  To change the width of components inside the div search for grid-cols-custom in tailwind.js and change corresponding attr.
     */
    <div className="grid grid-cols-custom h-screen">
      <SideDisplayList />
      <InfoDisplayList handleFocusedChat={handleFocusedChat} />

      <FocusedView focusedChat={focusedChat} />
    </div>
  );
};

export default LandingPage;
