import React, { useState } from "react";
import { useDispatch } from "react-redux";
import Up from "../../img/uploading.svg";
import Down from "../../img/multimedia-option.svg";
import * as actions from "../../actions/actions";

const Upvote = ({ comment, index }) => {
  const [votes, setVotes] = useState(comment.rating);
  const [voteUp, setVoteUp] = useState(false);
  const [voteDown, setVoteDown] = useState(false);
  const [inFlight, setInFlight] = useState(true);
  const dispatch = useDispatch();
  const [upDisabled, setUpDisabled]= useState(false);
  const [downDisabled, setDownDisabled] = useState(false);

  /* sends thread id and index for the comment and if its an upvote or not */
  /*  Added a rating: 0 parameter to each comment*/
  /* What does backend need for information to add upvotes? */
  React.useEffect(() => {
    if (voteUp === true && inFlight) {
      setVoteUp(false);
      setInFlight(false);
      dispatch(
        actions.upVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
      setInFlight(true);
    }
  }, [voteUp]);

  React.useEffect(() => {
    if (voteDown === true && inFlight) {
      setVoteDown(false);
      setInFlight(false);
      dispatch(
        actions.downVote({
          thread_id: comment.thread_id,
          index: index.toString(),
        })
      );
      setInFlight(true);
    }
  }, [voteDown]);

  return (
    <div>
      <img
        alt=""
        src={Up}
        className={!upDisabled ? "cursor-pointer h-4 w-4" : "h-4 w-4 vote-disabled"}
        onClick={() => {
          if(!upDisabled){
            setVoteUp(true);
            setVotes(votes + 1);
            if(downDisabled){
              setDownDisabled(false);
            }else{
              setUpDisabled(true);
            }
          }
        }}
      ></img>
      <div className="cursor-default ml-1">
        {votes}
      </div>
      <img
        alt=""
        src={Down}
        className={!downDisabled ? "cursor-pointer h-4 w-4" : "h-4 w-4 vote-disabled"}
        onClick={() => {
          if(!downDisabled){
            setVoteDown(true);
            setVotes(votes -1);
            if(upDisabled){
              setUpDisabled(false);
            }else{
              setDownDisabled(true);
            }
          }
        }}
      ></img>
    </div>
  );
};

export default Upvote;
