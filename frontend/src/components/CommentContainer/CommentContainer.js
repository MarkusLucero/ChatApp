import React, { useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import * as actions from "../../actions/actions";
import "../../assets/main.css";
import Comment from "../Comment/Comment";
import CommentField from "../CommentField/CommentField.js";

/**  
 * CommentContainer handles comment state and receives comment updates from the Comment through callback functions.
 * This updated comment is then passed to the CommentField in order to update CommentField which in turn displays the comment.
 * CommentContainer acts as a container for all components related to comments on threads
 * @returns The entire comment section (posted comments + comment box) for a forum thraed
 */

const CommentContainer = ({ thread }) => {
  /* Thread is passed in as a paprameter. Use its comments array as start array*/
  /* This state handles the comment input */
  const [comment, setComment] = useState("");
  /* This state handles if comment was posted or not */
  const [addComment, setAddComment] = useState(false);
  /* This state handles reply input */
  const [reply, setReply] = useState("");
  /* This state handles if reply was posted or not */
  const [addReply, setAddReply] = useState(false);
  /* Gets the username of the person posting the comment*/
  const poster = useSelector((state) => state.socketState.username);
  /* The recursive comment obj array */
  const [comments, setComments] = useState(thread.comments);
  /* Index to the comment we reply to */
  const [index, setIndex] = useState(0);

  /* This state is used as a dummy, we must always pass a dummy function to comment to allow it to use setReplyBox */
  const [dummyReply, setDummyReply] = useState([]);
  const dispatch = useDispatch();

  const [commentCounter, setCommentCounter] = useState(0);
  /* This function returns a comment object.  */
  function getParentId() {
    if (commentCounter === 0) {
      return 0;
    }
    return commentCounter - 1;
  }
  /* When we successfully change the value of comment, i.e someone comment button is pressed, we 
  dispatch an addComment action.  */
  React.useEffect(() => {
    let didCancel = false;
    function fetchData() {
      if (addComment === true) {
        dispatch(
          actions.addComment({
            thread_id: thread.id,
            index: commentCounter.toString(),
            reply_index: "",
            rating: "0",
            username: poster,
            comment: comment,
          })
        );
        setAddComment(false);
        setCommentCounter(commentCounter + 1);
        setComment("");
      }
    }

    fetchData();
    return () => {
      didCancel = true;
    };
  }, [addComment]);

  React.useEffect(() => {
    if (addReply === true) {
      dispatch(
        actions.addComment({
          thread_id: thread.id,
          index: commentCounter.toString(),
          reply_index: index.toString(),
          rating: 0,
          username: poster,
          comment: reply,
        })
      );
      setCommentCounter(commentCounter + 1);
    }
    setAddReply(false);
    setReply("");
  }, [addReply]);

  React.useEffect(() => {
    if (thread) {
      setComments(thread.comments);
      setCommentCounter(thread.comments.length);
    }
  }, [thread]);

  const handleInputChange = (event) => {
    setComment(event.target.value);
  };

  const handleReplyChange = (event) => {
    setReply(event.target.value);
  };

  /* When submitting a reply we must create a new Posted Comment field and concatinate this new pushed comment field with
  the parrent reply [] array. */

  return (
    <div className="flex flex-col content-center focused-view-custom-bg h-screen75 overflow-y-scroll">
      <div className=" w-full">
        <div className="">
          <div className="commentContainerBg">
            <span className="text-white pt-10 pl-4 " id="comments">
              Comments
            </span>
            <span className="text-white pt-10" id="comments_count">
              ({commentCounter})
            </span>
            <Comment
              setReplyBox={setDummyReply}
              inputHandler={handleInputChange}
              comment={comment}
              setAddComment={setAddComment}
              username={poster}
            ></Comment>
            {/* only render this part if there's comments */}
            {comments.length > 0 && (
              <CommentField
                setIndex={setIndex}
                inputHandler={handleReplyChange}
                setAddReply={setAddReply}
                addReply={addReply}
                username={poster}
                comment={comment}
                comments={comments}
                setComments={setComments}
              ></CommentField>
            )}
          </div>
        </div>
      </div>
    </div>
  );
};

export default CommentContainer;
