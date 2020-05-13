import React, { useState } from "react";
import { useSelector, useDispatch } from "react-redux";
import * as actions from "../../actions/actions";
import "../../assets/main.css";
import Comment from "../Comment/Comment";
import CommentField from "../CommentField/CommentField.js";

/* CommentContainer handles comment state and receives comment updates from the Comment through callback functions.
 This updated comment is then passed to the CommentField in order to update CommentField which in turn displays the comment
 
 CommentField must hold an array representing every parent comment, in this case an array of comment objects.
 The comment object must in turn be an object that holds the actual message, 
 the poster of the message and an array of comment objects that symbolize replies. */

/* This function returns a comment object.  */
function assembleComment(poster, postedComment) {
  return {
    username: poster,
    comment: postedComment,
    reply: [],
  };
}
function assembleReply(poster, postedComment, reply) {
  return {
    username: poster,
    comment: postedComment,
    reply: reply,
  };
}

const CommentContainer = ({ thread }) => {
  /* Thread is passed in as a paprameter. Use its comments array as start array*/
  /* This state handles the comment input */
  console.log(thread.comments);
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

  /* When we successfully change the value of comment, i.e someone comment button is pressed, we 
  dispatch an addComment action.  */
  React.useEffect(() => {
    if (addComment === true) {
      dispatch(
        actions.addComment({
          thread_id: thread.id,
          username: poster,
          comment: comment,
          reply: {},
        })
      );
      /*  setComments((comments) =>
        comments.concat(assembleComment(poster, comment))
      ); */
      setAddComment(false);
      setCommentCounter(commentCounter + 1);
      setComment("");
    }
  }, [addComment]);

  React.useEffect(() => {
    if (addReply === true) {
      /*  setComments((comments) =>
        comments.concat(
          assembleReply(poster, reply, {
            username: comments[index].username,
            comment: comments[index].comment,
          })
        )
      ); */
      dispatch(
        actions.addComment({
          thread_id: thread.id,
          username: poster,
          comment: reply,
          reply: {
            user_id: comments[index].user_id,
            comment: comments[index].comment,
          },
        })
      );
      setCommentCounter(commentCounter + 1);
    }
    setAddReply(false);
    setReply("");
  }, [addReply]);

  const handleInputChange = (event) => {
    setComment(event.target.value);
  };

  const handleReplyChange = (event) => {
    setReply(event.target.value);
  };
  console.log(comments);

  /* When submitting a reply we must create a new Posted Comment field and concatinate this new pushed comment field with
  the parrent reply [] array. */

  return (
    <div className="flex flex-col content-center focused-view-custom-bg">
      <div className="p-40">
        <div className="p-10 ">
          <div className="commentContainerBg">
            <span className="text-white pt-10 pl-20 " id="comments">
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
          </div>
        </div>
      </div>
    </div>
  );
};

export default CommentContainer;
