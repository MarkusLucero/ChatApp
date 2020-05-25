import React from "react";
import Up from "../../img/uploading.svg";
import Down from "../../img/multimedia-option.svg";

const Upvote = () => {
  return (
    <div>
      <img alt="" src={Up} className="h-4 w-4"></img>
      <div className="ml-1">0</div>
      <img alt="" src={Down} className="h-4 w-4"></img>
    </div>
  );
};

export default Upvote;
