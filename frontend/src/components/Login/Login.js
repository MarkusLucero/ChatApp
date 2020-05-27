import React, { useState } from "react";
import { useFormik, setNestedObjectValues } from "formik";

import * as actions from "../../actions/actions";
import { useDispatch } from "react-redux";
import Register from "../Register/Register";

const axios = require("axios");

//axios.get("http://localhost:8080/", { crossdomain: true })

/**
 * Login provides the layout and login of a user
 * A successfully validated form will redirect to LandingPage with pathname = "/start"
 * @returns a div containing the form to fill out and its validation
 */
const Login = () => {
  /* useDispatch from dispatch function from store */
  const dispatch = useDispatch();
  const [errorMsg, setErrorMsg] = useState(false);
  const [fade, setFade] = useState(false);
  const validate = (values) => {
    const errors = {};
    if (!values.Username) {
      setErrorMsg(false);
      errors.Username = "Required";
    }

    if (!values.Password) {
      setErrorMsg(false);
      errors.Password = "Required";
      return errors;
    }
  };

  console.log(setFade);
  React.useEffect(() => {
    setFade(document.getElementById("container"));
  }, []);

  const formik = useFormik({
    initialValues: { Username: "", Password: "" },
    validate,
    onSubmit: (values) => {
      axios
        .post(
          "/",
          JSON.stringify({
            action: "login",
            username: values.Username,
            password: values.Password,
          })
        )
        .then(function (response) {
          console.log(response);

          /* The response contains: status and a payload data: server/token */
          switch (response.status) {
            /* Login accepted */

            case 200: {
              const data = {
                response: response.data,
                username: values.Username,
              };

              /* Data should contain token & server */
              dispatch(actions.setServer("ws://localhost:8080/websocket"));
              dispatch(actions.loginSuccess(data));
              break;
            }
            case 404: {
              const data = response.data;
              dispatch(actions.loginFailure({ data }));
              break;
            }
            default:
              alert("missing memberid");
              break;
          }
        })
        .catch(function (error) {
          setErrorMsg(true);
        });
      /* Use the store reducer to dispatch login actions */
      dispatch(actions.login({ values }));
    },
  });
  return (
    <div
      className="flex justify-center w-screen h-screen"
      style={{
        backgroundImage: "url(" + require("../../background_night.png") + ")",
      }}
    >
      <div className="container" id="container">
        <div className="form-container sign-in-container ">
          <form
            className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
            onSubmit={formik.handleSubmit}
          >
            <p className=" justify-center text-4xl font-bold font-mono">
              Log in
            </p>
            <p className="font-mono text-xl ">
              Enter your login details to access Chat Up!
            </p>

            <div className="mb-4">
              <label
                className="block text-gray-700 text-sm font-bold mb-2"
                htmlFor="Username"
              >
                Username
              </label>
              <input
                className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                id="Username"
                name="Username"
                type="Text"
                onChange={formik.handleChange}
                onBlur={formik.handleBlur}
                value={formik.values.Username}
              ></input>
              <div>
                {formik.touched.Username && formik.errors.Username ? (
                  <div className="text-red-600">{formik.errors.Username}</div>
                ) : null}
              </div>
              <div>
                {errorMsg ? (
                  <label className="text-red-600">
                    Login failed! Try something else.
                  </label>
                ) : null}
              </div>
              <label
                className="block text-gray-700 text-sm font-bold mb-2"
                htmlFor="Password"
              >
                Password
              </label>
              <input
                className="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
                id="Password"
                name="Password"
                type="Password"
                onChange={formik.handleChange}
                onBlur={formik.handleBlur}
                value={formik.values.Password}
              ></input>
              <div>
                {formik.touched.Password && formik.errors.Password ? (
                  <div className="text-red-600">{formik.errors.Password}</div>
                ) : null}
              </div>
              <button className="formButton" type="submit">
                Login
              </button>
            </div>
          </form>
        </div>
        <div className="form-container sign-up-container">
          <Register container={fade}></Register>
        </div>
        <div className="overlay-container">
          <div className="overlay">
            <div className="overlay-panel overlay-left">
              <h1 className="intro-header text-2xl"> Hey there!</h1>
              <p className="form-paragraphs">
                To keep connected with us please login with your personal info
              </p>
              <button
                className="ghost formButton"
                id="signIn"
                onClick={() => fade.classList.remove("right-panel-active")}
              >
                Sign In
              </button>
            </div>
            <div className="overlay-panel overlay-right">
              <h1 className="intro-header text-2xl"> Welcome!</h1>
              <p className="form-paragraphs">
                Enter your personal details to join the Chat Up community!
              </p>
              <button
                className="ghost formButton"
                id="signUp"
                onClick={() => fade.classList.add("right-panel-active")}
              >
                Sign Up
              </button>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};

export default Login;
