import React, { useState } from "react";
import "../../assets/main.css";
import Comment from "../Comment/Comment";
import PostedComment from "../PostedComment/PostedComment";

/* Each reply must have its own replyBox state */

const CommentField = ({
  username,
  comments,
}) => {

  return (
    <div className=" pl-20 pr-20 pb-20  pt-10">
      <div className="commentField">
        <PostedComment username={username} comments={comments}></PostedComment>
      </div>
    </div>
  );
};

export default CommentField;
