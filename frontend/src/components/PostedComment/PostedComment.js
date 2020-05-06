import React from "react";
import "../../assets/main.css";
import Reply from "../Reply/Reply";

const PostedComment = ({
  username,
  comments,
}) => {
  return (
    <div className="w-full">
      <div>{comments.username}</div>
      <div className="text-white w-full">
        {comments.map((comment, index) => (
          <div className="border-l  w-full " key={index}>
            <div className="  mt-4 ml-4 text-blue-700">{comment.username}</div>
            <div className=" ml-4 "> {comment.text}</div>
            <Reply
              username={username}
            ></Reply>
          </div>
        ))}
      </div>
    </div>
  );
};

export default PostedComment;
