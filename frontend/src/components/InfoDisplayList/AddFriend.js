import React, { useState } from "react";

const AddFriend = ({
  userInput,
  handleInputChange,
  handleAddFriend,
  show,
  setShow,
}) => {
  const content = show && (
    <div className="addFriend-custom-overlay">
      <div className="addFriend-custom-modal">
        <div className="addFriend-custom-modal-body">
          <h1>ADD FRIEND</h1>
          <label htmlFor="AddFriend" className="text-base">
            {" "}
            Send a friend request by entering their Username.{" "}
          </label>
          <input
            id="AddFriend"
            className=" flex flex-wrap shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            type="text"
            onChange={handleInputChange}
            value={userInput}
          ></input>
          <button
            className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 border border-green-700 rounded"
            onClick={handleAddFriend}
          >
            Add
          </button>
          <button
            className="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 border border-red-700 rounded"
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
