import React, { useState, useEffect, useRef } from "react";
const axios = require("axios");

async function friendRequest({
  setAddSuccessful,
  userInput,
  currentFriends,
  requester,
  from,
  setErrorMsg,
  setShow,
}) {

  if (userInput !== "" && userInput !== requester && !currentFriends.includes(userInput)) {
    try {
      const response = await axios.post(
        "/",
        JSON.stringify({
          action: "friend_request",
          username: userInput,
          password: from,
        })
      );
      const data = await response;
      switch (data.status) {
        case 200: {
          setAddSuccessful(true);
          setErrorMsg(false);
          setShow(false);
          break;
        }
        case 403: {
          setErrorMsg(true);
          break;
        }
        default:
          setErrorMsg(true);
          break;
      }
    } catch (error) {
      console.log(error);
      setErrorMsg(true);
    }
  } else {
    setErrorMsg(true);
  }
}

const AddFriend = ({
  userInput,
  handleInputChange,
  setAddSuccessful,
  requester,
  currentFriends,
  show,
  setShow,
  from,
}) => {
  /* This state is local to the modal and displays error text if failed attempt */
  const [errorMsg, setErrorMsg] = useState(false);

  useEffect(() => {
    document.addEventListener("mousedown", handleClick);
    return () => {
      document.removeEventListener("mousedown", handleClick);
    };
  }, []);

  const monad = useRef();
  /*Handle click outside createThread modal */
  const handleClick = (e) => {
    if (show === true) {
      if (monad.current.contains(e.target)) {
        // inside click
        return;
      } else {
        setShow(false);
      }
    }
  };

  return (
    <div className="addFriend-custom-modal" ref={monad}>
      <div className="addFriend-custom-modal-body">
        <h1>ADD FRIEND</h1>

        {errorMsg ? (
          <label htmlFor="AddFriend" className="text-red-600">
            Friend request failed! Try something else.
          </label>
        ) : (
          <label htmlFor="AddFriend" className="text-base">
            {" "}
            Send a friend request by entering their Username.
          </label>
        )}

        <input
          id="AddFriend"
          className=" flex flex-wrap shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
          type="text"
          onChange={handleInputChange}
          value={userInput}
        ></input>
        {/* On add, do a friend request and update success state accordingly */}
        <button
          className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 border border-green-700 rounded"
          onClick={() =>
            friendRequest({
              setAddSuccessful,
              userInput,
              currentFriends,
              requester,
              from,
              setErrorMsg,
              setShow,
            })
          }
        >
          Add
        </button>
        <button
          className="bg-red-900 hover:bg-red-700 text-white font-bold py-2 px-4 border border-red-700 rounded"
          onClick={() => setShow(false)}
        >
          Close
        </button>
      </div>
    </div>
  );
};
export default AddFriend;
