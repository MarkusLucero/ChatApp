import React, { useState } from "react";
import { useDispatch } from "react-redux";
import Up from "../../img/uploading.svg";
import Down from "../../img/multimedia-option.svg";
import * as actions from "../../actions/actions";

const Upvote = ({ comment, index }) => {
  const [voteUp, setVoteUp] = useState(false);
  const [voteDown, setVoteDown] = useState(false);
  const dispatch = useDispatch();

  /* sends thread id and index for the comment,  */
  /*  Added a rating: 0 parameter to each comment and reply object*/
  /* What does backend need for information to add upvotes? */
  React.useEffect(() => {
    if (voteUp === true) {
      dispatch(
        actions.upVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
    }
    setVoteDown(false);
  }, [voteUp]);
  React.useEffect(() => {
    if (voteDown === true) {
      dispatch(
        actions.downVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
    }
    setVoteUp(false);
  }, [voteDown]);

  return (
    <div>
      <img
        alt=""
        src={Up}
        className="h-4 w-4"
        onClick={() => {
          setVoteUp(true);
          setVoteDown(false);
        }}
      ></img>
      <div className="ml-1">
        {voteUp ? comment.rating + 1 : ""}
        {voteDown ? comment.rating - 1 : ""}
        {voteUp || voteDown ? "" : comment.rating}
      </div>
      <img
        alt=""
        src={Down}
        className="h-4 w-4"
        onClick={() => {
          setVoteUp(false);
          setVoteDown(true);
        }}
      ></img>
    </div>
  );
};

export default Upvote;
