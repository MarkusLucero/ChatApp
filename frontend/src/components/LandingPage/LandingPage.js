import React from "react";
import "../../assets/main.css";
import SideDisplayList from "../SideDisplayList/SideDisplayList";
import FocusedView from "../FocusedView/FocusedView";
import InfoDisplayList from "../InfoDisplayList/InfoDisplayList";
import { useSelector } from "react-redux";

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

  //we dont have a list of servers but only one server we can get this obj from redux store and pass it down
  const server = useSelector(state => state.socketState.server)

  /* state to check what page we are focusing on - some server or the home page*/
  /* Focusing on Home always on start*/
  const [focusedPage, setFocusedPage] = React.useState("Home");

  /* callback function for getting the id of the page that we are clicking on */
  const handleFocusedPage = (event) => {
    setFocusedPage(event.target.id)
  };

  return (
    /**
     *  The styling is made up of Tailwind css classes
     *  To change the width of components inside the div search for grid-cols-custom in tailwind.js and change corresponding attr.
     */
    <div className="grid grid-cols-custom h-screen">
      <SideDisplayList handleFocusedPage={handleFocusedPage} server={server}/>
      <InfoDisplayList handleFocusedChat={handleFocusedChat} focusedPage={focusedPage} />

      <FocusedView focusedChat={focusedChat} focusedPage={focusedPage} />
    </div>
  );
};

export default LandingPage;
