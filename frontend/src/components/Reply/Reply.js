import React, { useState } from "react";
import "../../assets/main.css";
import Comment from "../Comment/Comment";

const Reply = ({username}) => {
  const [replyBox, setReplyBox] = useState(false);

  function ToggleReplyBox() {
    if (replyBox) setReplyBox(false);
    else setReplyBox(true);
  }

  return (
    <div>
      <button className="ml-4 text-gray-600" onClick={ToggleReplyBox}>
        reply
      </button>
      {replyBox ? <Comment username={username}></Comment> : null}
    </div>
  );
};

export default Reply;
