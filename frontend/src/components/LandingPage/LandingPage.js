import React from "react";
import "../../assets/main.css"

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
      {/* TODO These divs will be replaced by Side display list, info display list and focused view components respectively hence the id:s*/}
      
      <div id="side-display-list" className="bg-blue-200"></div>
      <div id="info-display-list" className="bg-blue-400"></div>
      <div id="focused-view" className="bg-blue-600"></div>
    </div>
  );
};

export default LandingPage;