import React, { useState } from "react";

const Message = ({ message }) => {
  return (
    <div className={"flex h-10 mb-1" + (message.self ? ' justify-end' : ' justify-start')}>
      <div className="h-full p-2 rounded-full static bg-gray-400 center ">
        {message.message}
      </div>
    </div>
  );
};

export default Message;
