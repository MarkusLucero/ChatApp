import React, { useState } from "react";
import "../../assets/main.css";
import Comment from "../Comment/Comment";

const Reply = ({ username, inputHandler, setAddReply, index, setIndex }) => {
  const [replyBox, setReplyBox] = useState(false);

  function ToggleReplyBox() {
    if (replyBox) setReplyBox(false);
    else {
      setReplyBox(true);
      setIndex(index);
    }
  }
  return (
    <div>
      <button className="text-gray-600" onClick={ToggleReplyBox}>
        reply
      </button>
      {replyBox ? (
        <Comment
          setReplyBox={setReplyBox}
          inputHandler={inputHandler}
          setAddComment={setAddReply}
          username={username}
        ></Comment>
      ) : null}
    </div>
  );
};

export default Reply;
