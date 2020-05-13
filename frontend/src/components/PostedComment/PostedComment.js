import React from "react";
import "../../assets/main.css";
import Reply from "../Reply/Reply";
import placeholder from "../../img/placeholder.png";

const PostedComment = ({
  username,
  inputHandler,
  setAddReply,
  addReply,
  comments,
  setComments,
  setIndex,
}) => {
  return (
    <div className="w-full">
      <div>{comments.username}</div>
      <div className="text-white w-full">
        {comments.map((comment, index) => (
          <div className=" w-full " key={index}>
            <span className="">
              <img
                className="rounded-full w-12 h-auto  mb-4 mr-4 float-left"
                alt=""
                src={placeholder}
              ></img>
            </span>
            <div className="break-words ml-12">
              <div className="  mt-4 ml-4 text-blue-700">{comment.user_id}</div>
              {Object.keys(comment.reply).length !== 0 ? (
                <div className="quote">
                  <div className=" font-bold ml-4">
                    Originally posted by {""}
                    {console.log(comment.reply)}
                    <span className="font-bold text-blue-700">
                      {comment.reply.user_id}
                    </span>
                  </div>
                  <div className="break"></div>
                  <div className="ml-4 ">{comment.reply.user_id}</div>
                </div>
              ) : null}
              <div className=" ml-4 ">
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
