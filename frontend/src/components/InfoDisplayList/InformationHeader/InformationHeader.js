import React from "react";
import placeholder from "../../../img/placeholder.png";

/**
 * contains information about the User if on Home Page or the specific server we are on
 * @property {String} username is the username of logged in user
 * @property {String} focusedPage - a string used to check what page we are focusing on
 * returns a div containing the appropriate information to be displayed
 */
const InformationHeader = ({ username, focusedPage }) => {
  return (
    <div className="mb-5">
      <div
        id="informationHeader"
        className="h-20 text-4xl flex p-2 flex-auto mr-4 text-white  mb-10"
      >
        {focusedPage === "Home" ? (
          <div className="flex flex-row">
            <img
              className="rounded-full w-16 h-auto mr-5"
              alt=""
              src={placeholder}
            />
            <h1>{username}</h1> 
          </div>
        ) : (
          <h1> {focusedPage}</h1>
        )}
      </div>
    </div>
  );
};

export default InformationHeader;
