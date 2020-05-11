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
  
  /*state to check what thread we are focuing on */
  const [focusedThread, setFocusedThread] = React.useState(null);
  

  /* callback function for getting the id of the direct message div that we are clicking on */
  const handleFocusedChat = (event) => {
    setFocusedChat(event.target.id);
  };

  /*callback for focusing a thread */ 
  const handleFocusedThread = (event) => {
    setFocusedThread(event.target.id);
  
  }

  /* The global server object */
  const [server, setServer] = React.useState({});

  //server object from redux
  const serverObject = useSelector(state => state.socketState.server);

  React.useEffect(() => {
    if (serverObject !== null) {
      setServer(serverObject);
    }
  }, [serverObject]);

  /* state to check what page we are focusing on - some server or the home page*/
  /* Focusing on Home always on start*/
  const [focusedPage, setFocusedPage] = React.useState("Home");

  /* callback function for getting the id of the page that we are clicking on */
  const handleFocusedPage = (event) => {
    setFocusedPage(event.target.id)
  };

  return (
    <div className="grid grid-cols-custom h-screen">
      <SideDisplayList handleFocusedPage={handleFocusedPage} server={server}/>
      <InfoDisplayList handleFocusedChat={handleFocusedChat} focusedPage={focusedPage} 
      handleFocusedThread = {handleFocusedThread}  />

      <FocusedView focusedChat={focusedChat} focusedPage={focusedPage} focusedThread ={focusedThread} />
    </div>
  );
};

export default LandingPage;
