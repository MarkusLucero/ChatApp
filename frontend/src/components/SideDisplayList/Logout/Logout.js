import React from "react";
import * as actions from "../../../actions/actions";
import { useDispatch, useSelector } from "react-redux";
import logout from "../../../img/logout.svg";
import LogoutMonad from "./LogoutMonad";
import ReactTooltip from "react-tooltip";
const axios = require("axios");

/**
 * Handle the logout http response with backend
 * @param {String} username the username of the user
 * @param {String} magicToken the magictoken used for authentication on websocket connection
 * @param {function} setLogoutSuccess callback function used to set state of bool upon logout
 */
const logoutHandler = async (username, magicToken, setLogoutSuccess) => {
  try {
    const response = await axios.post(
      "/",
      JSON.stringify({
        action: "logout",
        username: username,
        magic_token: magicToken,
      })
    );
    const data = await response;
    switch (data.status) {
      case 200: {
        setLogoutSuccess(true);
        break;
      }
      case 404: {
        break;
      }
      default:
        break;
    }
  } catch (error) {
    console.log(error);
  }
};

/**
 * Component that handles the logout functionality
 * @returns clickable button that will logout the user
 */
const Logout = () => {
  const dispatch = useDispatch();
  const magicToken = useSelector((state) => state.socketState.magicToken);
  const username = useSelector((state) => state.socketState.username);
  const [logoutSuccess, setLogoutSuccess] = React.useState(false);
  const [hovered, setHovered] = React.useState(false);

  const [logoutMonad, setLogoutMonad] = React.useState(false);

  React.useEffect(() => {
    if (logoutSuccess === true) {
      dispatch(actions.logOut()); /* Reset socketState */
      dispatch(
        actions.resetLoginState()
      ); /* Reset loginState  -- this will trigger redirect to login page! */
      console.log("Successfuly logged out!");
    }
  }, [logoutSuccess, dispatch]);

  return (
    <div className="text-white cursor-pointer flex flex-row justify-center  mb-2">
      <img
        alt={logout}
        data-tip
        data-for="logoutUser"
        src={logout}
        onMouseEnter={() => {
          setHovered(!hovered);
        }}
        onMouseLeave={() => {
          setHovered(!hovered);
        }}
        className={
          hovered
            ? "plusIcon-custom-hover h-8 w-8 cursor-pointer"
            : "h-8 w-8 cursor-pointer"
        }
        onClick={() => {
          setLogoutMonad(true);
        }}
        alt="logout button"
      />
      <ReactTooltip
        id="logoutUser"
        place="right"
        effect="solid"
        backgroundColor="black"
      >
        Logout
      </ReactTooltip>
      {logoutMonad ? (
        <div className="addFriend-custom-overlay">
          <LogoutMonad
            logoutHandler={logoutHandler}
            setLogoutMonad={setLogoutMonad}
            username={username}
            magicToken={magicToken}
            setLogoutSuccess={setLogoutSuccess}
          />
        </div>
      ) : null}
    </div>
  );
};

export default Logout;
