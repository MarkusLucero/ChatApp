import React, { useState } from "react";
const axios = require("axios");

async function friendRequest({
  setAddSuccessful,
  userInput,
  from,
  setErrorMsg,
}) {
  if (userInput !== "") {
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
          break;
        }
        case 404: {
          setErrorMsg(true);
          break;
        }
        default:
          setErrorMsg(true);
          break;
      }
    } catch (error) {
      console.log(error);
    }
  } else {
    setErrorMsg(true);
  }
}

const AddFriend = ({
  userInput,
  handleInputChange,
  setAddSuccessful,
  show,
  setShow,
  from,
}) => {
  /* This state is local to the modal and displays error text if failed attempt */
  const [errorMsg, setErrorMsg] = useState(false);

  const content = show && (
    <div className="addFriend-custom-overlay">
      <div className="addFriend-custom-modal">
        <div className="addFriend-custom-modal-body">
          <h1>ADD FRIEND</h1>

          {errorMsg ? (
            <label htmlFor="AddFriend" className="text-red-600">
              {" "}
              Something went wrong! Try something else.
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
              friendRequest({ setAddSuccessful, userInput, from, setErrorMsg })
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
    </div>
  );

  return content;
};
export default AddFriend;
