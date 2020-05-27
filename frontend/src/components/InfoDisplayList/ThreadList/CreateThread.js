import React, { useState, useRef, useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import * as actions from "../../../actions/actions";

/**
 * CreateThread is the component which is seen when pressing '+' icon next to active threads
 * @param {bool} showCreateThread a Boolean indicating if this monad is visible or not
 * @param {function} setShowCreateThread callback is used to show/hide the createThread modal
 * @returns a modal on which the user can create a new thread
 */
const CreateThread = ({ showCreateThread, setShowCreateThread }) => {

  /* Add event listener so we can detect clicks outside create thread modal */
  useEffect(() => {
    document.addEventListener("mousedown", handleClick);
    return () => {
      document.removeEventListener("mousedown", handleClick);
    };
  });

 /**
 * handles a click outside of the create chat modal
 * @param event of the window object
 */
  const handleClick = (event) => {
    if (showCreateThread === true) {
      if (monad.current.contains(event.target)) {
        // inside click
        return;
      } else {
        setShowCreateThread(false);
      }
    }
  };

  const monad = useRef();

  /*get the current server */
  const server = useSelector((state) => state.socketState.server);

  /*get the currently logged in user*/
  const username = useSelector((state) => state.socketState.username);

  /*Local state for the summary*/
  const [summary, setSummary] = useState("");

  /*Local state for the details */
  const [details, setDetails] = useState("");

  /* useDispatch from dispatch function from store */
  const dispatch = useDispatch();

  const createThread = () => {
    if (summary !== "" && details !== "") {
      const data = {
        server: server,
        user: username,
        summary: summary,
        details: details,
      };
      dispatch(actions.createThread(data));
      setShowCreateThread(false);
    } else {
      setShowCreateThread(false);
    }
  };

  return (
    <div className="addFriend-custom-modal" ref={monad}>
      <div className="addFriend-custom-modal-body">
        <label htmlFor="AddFriend" className="text-base">
          {" "}
          <h1 className="text-white">Enter the header for your thread: </h1>
        </label>
        <input
          id="Summary"
          placeholder="Summary..."
          onChange={(e) => {
            setSummary(e.target.value);
          }}
          className=" flex flex-wrap shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
          type="text"
        ></input>
        <h1 className="mt-2 text-white">Enter a detailed description:</h1>

        <textarea
          onChange={(e) => {
            setDetails(e.target.value);
          }}
          placeholder="Details.."
          type="text"
          className="mb-2 h-20 flex flex-warp shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
        ></textarea>
        <button
          onClick={createThread}
          className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 border border-green-700 rounded"
        >
          Create
        </button>
        <button
          className="bg-red-900 hover:bg-red-700 text-white font-bold py-2 px-4 border border-red-700 rounded"
          onClick={() => {
            setShowCreateThread(false);
          }}
        >
          Close
        </button>
      </div>
    </div>
  );
};

export default CreateThread;
