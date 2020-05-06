import React, {useRef} from "react";
import plus from "../../img/plus.svg";
import CreateThread from "./CreateThread";

const ThreadList = ({ focusedPage, username }) => {
  /* used for the hover effect on + sign */
  const [hovered, setHovered] = React.useState(false);
  const [showCreateThread, setShowCreateThread] = React.useState(false);
  const createThread = useRef();
  return (
    <div className="flex flex-col ml-2">
      <div
        id="threadListHeader"
        className="text-white text-xl h-auto border-solid border-b-2 border-gray-700  mt-4 flex flex-row justify-between"
      >
        Your Threads
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
          onClick ={()=>{setShowCreateThread(true)}}
        />
      </div>
      <div ref={createThread} >
        {showCreateThread ? <CreateThread/> : null}
      </div>
      <div id="threadList" className="text-white pt-1">
        list of threads created by you 
        {/* MAP a list of thread components here */}
      </div>
    </div>
  );
};

export default ThreadList;
