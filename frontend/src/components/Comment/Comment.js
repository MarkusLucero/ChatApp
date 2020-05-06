import React, { useState } from "react";
import "../../assets/main.css";

const Comment = ({ inputHandler, comment, setAddComment, username }) => {
  return (
    <div className=" pl-20 pr-20 ">
      <textarea
        placeholder="Enter your comment here"
        rows="3"
        value={comment}
        onChange={inputHandler}
      ></textarea>

      <div className="panel">
        <div className=" text-white">
          Comment as <span className="text-blue-700">{username}</span>
        </div>
        <button
          className=" ml-auto bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 border border-blue-700 rounded"
          onClick={() => setAddComment(true)}
        >
          Comment{" "}
        </button>
      </div>
    </div>
  );
};

export default Comment;
