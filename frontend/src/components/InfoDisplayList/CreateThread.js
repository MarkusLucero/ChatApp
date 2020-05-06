import React from "react";

const CreateThread = () => {
  return (
    <div
      style={{ left: "24rem", top: "16rem" }}
      className=" w-56 z-40 rounded mt-2 bg-gray-700 absolute p-2  w-auto"
    >
      <div className="text-white border-solid border-b border-black mb-2 pr-3">
        Choose friends to add:
      </div>
      
      <input
        className=" mt-2 text-white input-box-custom-bg shadow appearance-none "
        type="text"

        placeholder="Chat name..."
      />
      <button
        className="rounded w-full bg-blue-600 mt-4 cursor-pointer"

      >
        Create thread
      </button>
    </div>
  );
};

export default CreateThread;
