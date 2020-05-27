import React from "react";
import "../../assets/main.css";
import PostedComment from "../PostedComment/PostedComment";

/* Each reply must have its own replyBox state */

const CommentField = ({
  inputHandler,
  setAddReply,
  addReply,
  username,
  comments,
  setIndex,
  setComments,
}) => {
  return (
    <div className=" pl-4 pr-4 pb-10  pt-10">
      <div className="commentField">
        <PostedComment
         className="text-black"
          setIndex={setIndex}
          addReply={addReply}
          username={username}
          inputHandler={inputHandler}
          setAddReply={setAddReply}
          comments={comments}
          setComments={setComments}
        ></PostedComment>
      </div>
    </div>
  );
};

export default CommentField;
