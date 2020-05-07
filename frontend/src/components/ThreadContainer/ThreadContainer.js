import React from "react";
import SearchBar from "../SearchBar/SearchBar";
import OriginalPost from "./OriginalPost";

/**
 * ThreadContainer holds all information about a thread; rootpost, comments etc
 * @property {string} focusedThread the threadId of the thread we're currently focusing on 
 * @returns a div with the Originalpost and comments
 */
const ThreadContainer = ({focusedThread }) => {
  /* State and callback functions for the SearchBar */
  const [searchTerm, setSearchTerm] = React.useState("");

  /**
   * Set the state of searchTerm to the value of the input field when that changes
   * @param event the event object of the window
   */
  const handleSearchInput = (event) => {
    setSearchTerm(event.target.value);
  };

  /**
   * Trigger the search of the searchTerm in the actuall focused chat
   * event parameter is displayed only to prevent he windows default action when pressing a submit button
   * @param event the event object of the window
   */
  const handleSearchSubmit = (event) => {
    /* TODO Actually search for threads containing the text in searchTerm and only allow that
     to be displayed in the threadContainer
  */
    console.log(searchTerm);
    setSearchTerm("");
    event.preventDefault();
  };

  /* TODO! fix that we display a certain thread here or all threads of server to pick from?? */

  return (
    <div className="focused-view-custom-bg text-white flex flex-col content-center ">
      <SearchBar
        id="search-chat"
        value={searchTerm}
        onButtonClick={handleSearchSubmit}
        onInputChange={handleSearchInput}
      />
      <div className="h-screen75">

      {focusedThread ? <OriginalPost focusedThread={focusedThread} /> : null}
      </div>
    </div>
  );
};

export default ThreadContainer;
