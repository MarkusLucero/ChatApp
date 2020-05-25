import React, { useState } from "react";
import { useDispatch } from "react-redux";
import Up from "../../img/uploading.svg";
import Down from "../../img/multimedia-option.svg";
import * as actions from "../../actions/actions";

const Upvote = ({ comment, index }) => {
  const [votes, setVotes] = useState(comment.rating);
  const [voteUp, setVoteUp] = useState(false);
  const [voteDown, setVoteDown] = useState(false);
  const [inFlight, setInFlight] = useState(false);
  const dispatch = useDispatch();

  /* sends thread id and index for the comment and if its an upvote or not */
  /*  Added a rating: 0 parameter to each comment*/
  /* What does backend need for information to add upvotes? */
  React.useEffect(() => {
    if (voteUp === true && inFlight === false) {
      setInFlight(true);
      dispatch(
        actions.upVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
      setInFlight(false);
    }
  }, [voteUp]);

  React.useEffect(() => {
    if (voteDown === true && !inFlight) {
      setInFlight(true);
      dispatch(
        actions.downVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
      setInFlight(false);
    }
  }, [voteDown]);

  return (
    <div>
      <img
        alt=""
        src={Up}
        className="cursor-pointer h-4 w-4"
        onClick={() => {
          setVoteUp(!voteUp);
          setInFlight(true);
          setVoteDown(false);
        }}
      ></img>
      <div className="cursor-default ml-1">
        {voteUp ? votes + 1 : ""}
        {voteDown ? votes - 1 : ""}
        {voteUp || voteDown ? "" : votes}
      </div>
      <img
        alt=""
        src={Down}
        className="cursor-pointer h-4 w-4"
        onClick={() => {
          setVoteDown(!voteDown);
          setInFlight(true);
          setVoteUp(false);
        }}
      ></img>
    </div>
  );
};

export default Upvote;
