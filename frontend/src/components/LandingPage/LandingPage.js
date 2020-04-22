import React from "react";
import "../../assets/main.css"
import SideDisplayList from "../SideDisplayList/SideDisplayList";
import FocusedView from "../FocusedView/FocusedView";
import InfoDisplayList from "../InfoDisplayList/InfoDisplayList";
/**
 * LandingPage holds the layout design grid of the app.
 * 
 * @returns a div containing the SideDisplayList, InfoDisplayList and FocusedView components 
 */
const LandingPage = () => {
  return (
      /**
       *  The styling is made up of Tailwind css classes 
       *  To change the width of components inside the div search for grid-cols-custom in tailwind.js and change corresponding attr.
       */
    <div className="grid grid-cols-custom h-screen">
      <SideDisplayList/>
      <InfoDisplayList/>

      <FocusedView />
    </div>
  );
};

export default LandingPage;