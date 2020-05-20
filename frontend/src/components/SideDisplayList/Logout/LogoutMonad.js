import React, { useRef, useEffect } from "react";

const LogoutMonad = ({
  logoutHandler,
  setLogoutMonad,
  username,
  magicToken,
  setLogoutSuccess,
}) => {
  /* Add event listener so we can detect clicks outside create thread modal */
  useEffect(() => {
    document.addEventListener("mousedown", handleClick);
    return () => {
      document.removeEventListener("mousedown", handleClick);
    };
  });

  /**
   * handles a click outside of the create chat modal
   * @param event of the window object
   */
  const handleClick = (event) => {
    if (monad.current.contains(event.target)) {
      // inside click
      return;
    } else {
      setLogoutMonad(false);
    }
  };

  const monad = useRef();

  return (
    <div className="logout-custom-modal" ref={monad}>
      <div className="addFriend-custom-modal-body">
        <label htmlFor="AddFriend" className="text-base">
          {" "}
          <h1 className="text-white">Are you sure you want to log out ?</h1>
        </label>
        <div className="flex flex-row justify-around mt-4">
          <button
            onClick={() =>
              logoutHandler(username, magicToken, setLogoutSuccess)
            }
            className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 border border-green-700 rounded"
          >
            Yes
          </button>
          <button
            className="bg-red-900 hover:bg-red-700 text-white font-bold py-2 px-4 border border-red-700 rounded"
            onClick={() => {
              setLogoutMonad(false);
            }}
          >
            No
          </button>
        </div>
      </div>
    </div>
  );
};

export default LogoutMonad;
