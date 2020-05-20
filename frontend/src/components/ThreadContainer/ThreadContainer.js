import React from "react";
import SearchBar from "../SearchBar/SearchBar";
import OriginalPost from "./OriginalPost";
import {useSelector} from "react-redux"
import CommentContainer from "../CommentContainer/CommentContainer"
import ThreadInfo from "./ThreadInfo";
/**
 * ThreadContainer holds all information about a thread; rootpost, comments etc
 * @property {string} focusedThread the threadId of the thread we're currently focusing on 
 * @property {Function} handleFocusedThread - callback funktion for setting the focused thread
 * @property {function} resetFocusedThread callback for resetting the focused thread
 * @property {boolean} threadLock used for conditional rendering of threads
 * @returns a div with the Originalpost and comments
 */
const ThreadContainer = ({focusedThread, handleFocusedThread, resetFocusedThread, threadLock }) => {
  /* State and callback functions for the SearchBar */
  const [searchTerm, setSearchTerm] = React.useState("");


  /**
   * Set the state of searchTerm to the value of the input field when that changes
   * @param event the event object of the window
   */
  const handleSearchInput = (event) => {
    setSearchTerm(event.target.value);
  };

    
    // list of all threads in server object from redux store
    const listOfThreads = useSelector(
      (state) => state.socketState.server.listOfThreads
    );
  
    //helper function for retrieving the right thread from state
    const rightThread = (list) => {
        for (const thread of list) {
          if (thread.id === focusedThread) {
            return thread;
          }
        }
      return null;
    };
  
    /*local state for actual thread object*/
    const [thread, setThread] = React.useState(rightThread(listOfThreads));
    /* state for thread comments */
  
    /*update the focused thread when clicked, refire when focusedThread or state's thread is updated */
    React.useEffect(() => {
      if (listOfThreads != null) {
        const actual = rightThread(listOfThreads);
        setThread(actual);
      }
    }, [focusedThread, listOfThreads]);
  return (
    <div className="focused-view-custom-bg text-white flex flex-col content-center ">
     {/*<SearchBar
        id="search-chat"
        value={searchTerm}
        onButtonClick={handleSearchSubmit}
        onInputChange={handleSearchInput}
      /> */}
      
      <div className="h-screen75">
      { !focusedThread && threadLock ? 
        listOfThreads.map((thread, index) => {
          return <ThreadInfo handleFocusedThread={handleFocusedThread}thread={thread} key={index}/>
      }):null }
  
      {focusedThread && thread ? <OriginalPost handleFocusedThread={handleFocusedThread} thread={thread} focusedThread={focusedThread} resetFocusedThread={resetFocusedThread} /> : null}
      {focusedThread && thread ? <CommentContainer thread={thread} focusedThread={focusedThread} /> : null}
      </div>
    </div>
  );
};

export default ThreadContainer;
