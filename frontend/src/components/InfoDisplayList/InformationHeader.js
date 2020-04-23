import React from "react";
import placeholder from "../../img/placeholder.png";


/**
 * contains information about the User if on Home Page or (TODO) the specific server we are on
 *
 * @param {string} username is the username of logged in user
 * returns a div containing the appropriate informaiton
 */
const InformationHeader = ({ username }) => {
  return (
    <div className ="mb-5">
      <div
        id="informationHeader"
        className="h-20 text-4xl flex p-2 flex-auto mr-4 text-white  mb-10"
      >
        <img
          className="rounded-full w-16 h-auto mr-5"
          alt=""
          src={placeholder}
        />
        {username}
      </div>
    </div>
  );
};

export default InformationHeader;