import React from "react";
import * as actions from "../../../actions/actions";
import { useDispatch, useSelector } from "react-redux";

const axios = require("axios");

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
        console.log("200");
        setLogoutSuccess(true);
        break;
      }
      case 404: {
        console.log("404");
        break;
      }
      default:
        console.log("default catch");
        break;
    }
  } catch (error) {
    console.log(error);
  }
};

const Logout = () => {
  const dispatch = useDispatch();
  const magicToken = useSelector((state) => state.socketState.magicToken);
  const username = useSelector((state) => state.socketState.username);
  const [logoutSuccess, setLogoutSuccess] = React.useState(false);

  React.useEffect(() => {
    if (logoutSuccess === true) {
      dispatch(actions.logOut()); /* Reset socketState */
      dispatch(actions.resetLoginState()) /* Reset loginState  -- this will trigger redirect to login page! */
      console.log("Successfuly logged out!")
    }
  }, [logoutSuccess, dispatch]);

  return (
    <div
      className="text-white cursor-pointer"
      onClick={() => logoutHandler(username, magicToken, setLogoutSuccess)}
    >
      LOGOUT
    </div>
  );
};

export default Logout;
