import React from "react";
import plus from "../../img/plus.svg";
import { useSelector } from "react-redux";


const ThreadList = ({ focusedPage, username }) => {
  /* used for the hover effect on + sign */
  const [hovered, setHovered] = React.useState(false);

  /* used for mapping threads in the threadlist component so they are displayed*/
  const [threads, setThreads] = React.useState([])
  
  // list of all threads in server object from redux store
  const listOfThreads = useSelector(state => state.socketState.server.listOfThreads);


  //if listOfThreads changes add it to threads state so it can be displayed
  React.useEffect(() => {
    if (listOfThreads != null) {
      setThreads(listOfThreads);
    }
  }, [listOfThreads]);

  return (
    <div className="flex flex-col ml-2">
      <div
        id="threadListHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700  mt-4 flex flex-row justify-between"
      >
        Threads
        <img
          src={plus}
          onMouseEnter={() => {
            setHovered(!hovered);
          }}
          onMouseLeave={() => {
            setHovered(!hovered);
          }}
          alt="Create a new thread!"
          className={hovered ? "plusIcon-custom-hover h-6 w-6 cursor-pointer" : "h-6 w-6 cursor-pointer"}
        />
      </div>
      <div id="threadList" className="text-white pt-1 h-screen25 overflow-y-scroll">
        a list of all threads in this server
        {/* MAP a list of thread components that exist in the server object  */}
        {threads.map((thread, index) => (
        "thread func component here ..."
      ))}
      </div>
    </div>
  );
};

export default ThreadList;
