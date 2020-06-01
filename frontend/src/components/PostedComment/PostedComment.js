import React from "react";
import "../../assets/main.css";
import Reply from "../Reply/Reply";
import placeholder from "../../img/placeholder.png";
import Upvote from "../Upvote/Upvote";

/**
 * PostedComment maps and renders all comments from the comment array associated with the current thread
 * @returns a div with all comments rendered
 */

const PostedComment = ({
  username,
  inputHandler,
  setAddReply,
  comments,
  setComments,
  setIndex,
}) => {
  return (
    <div className="w-full">
      <div>{comments.user_id}</div>
      <div className="text-white w-full">
        {comments.map((comment, index) => (
          <div className=" w-full " key={index}>
            <span className=" inline-flex float-left">
              <div className="mr-4">
                <Upvote comment={comment} index={index}></Upvote>
              </div>
              <img
                className="rounded-full w-12 h-auto  mb-4 mr-4 float-left"
                alt=""
                src={placeholder} 
              ></img>
            </span>
            <div className="break-words ml-12">
              <div className="  mt-4 ml-4 text-blue-700">{comment.user_id}</div>
              {Object.keys(comment.reply.reply_comment).length !== 0 ? (
                <div className="quote">
                  <div className=" font-bold ml-8 ">
                    Originally posted by {""}
                    <span className="font-bold text-blue-700">
                      {comment.reply.reply_user}
                    </span>
                  </div>
                  <div className="break"></div>
                  <div className="ml-4 ">{comment.reply.reply_comment}</div>
                </div>
              ) : null}
              <div className=" ml-12 ">
                {" "}
                {comment.comment}
                <Reply
                  setIndex={setIndex}
                  inputHandler={inputHandler}
                  index={index}
                  setAddReply={setAddReply}
                  username={username}
                  comments={comments}
                  setComments={setComments}
                ></Reply>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
};

export default PostedComment;
