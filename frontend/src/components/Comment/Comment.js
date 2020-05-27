import React from "react";
import "../../assets/main.css";

/**
 * Comment holds all input for creating a new comment aswell as the button for sending the comment
 * @property {Function} inputHandler callback for updating the comment text
 * @property {String} comment - the comment 
 * @property {Function} setAddComment callback for sending a comment
 * @property {String} username the logged in username
 * @property {Function} setReplyBox callback for setting the reply box
 * @returns a div with a comment text input aswell as button for sending
 */
const Comment = ({ inputHandler, comment, setAddComment, username, setReplyBox}) => {

    function onClickHandler(){
        setAddComment(true);
        setReplyBox(false);
    }

  return (
    <div className="pl-4 pr-4 ">
      <textarea
        placeholder="Enter your comment here"
        rows="3"
        wrap='hard'
        value={comment}
        onChange={inputHandler}
      ></textarea>

      <div className="panel">
        <div className=" text-white">
          Comment as <span className="text-blue-700">{username}</span>
        </div>
        <button
          className=" ml-auto bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 border border-blue-700 rounded"
          onClick={onClickHandler}
        >
          Comment{" "}
        </button>
      </div>
    </div>
  );
};

export default Comment;
