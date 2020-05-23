import React, { useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import Up from "../../img/uploading.svg";
import Down from "../../img/multimedia-option.svg";

const Upvote = ({comment}) => {
  const [votes, setVotes] = useState(0);
  const [voteUp, setVoteUp] = useState(false);
  const [voteDown, setVoteDown] = useState(false);
  const [hasVoted, setHasVoted] = useState(false);

  React.useEffect(() => {}, [voteUp,voteDown]);

  return (
    <div>
      <img
        alt=""
        src={Up}
        className="h-4 w-4"
        onClick={() => {
          setHasVoted(true);
          setVoteUp(true);
          setVoteDown(false);
        }}
      ></img>
      <div className="ml-1">
        {voteUp ? votes + 1 : ""}
        {voteDown ? votes - 1 : ""}
        {voteUp || voteDown ? "" : votes}
      </div>
      <img
        alt=""
        src={Down}
        className="h-4 w-4"
        onClick={() => {
          setHasVoted(true);
          setVoteUp(false);
          setVoteDown(true);
        }}
      ></img>
    </div>
  );
};

export default Upvote;
