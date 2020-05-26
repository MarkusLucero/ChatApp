import React, { useState } from "react";
import { useFormik } from "formik";
import * as Notifications from "../Notifications/Notifications";

/* import Notifications from "../Notifications/Notifications";
 */
const axios = require("axios");

/**
 * Register provides the layout and the registration for an user account
 * A successfully validated form will redirect to login page pathname = "/"
 * @property history is passed on due to Register being an immediate child of a Route component in Container component
 * @returns a div containing the form to fill out and its validation
 */
const Register = ({ container }) => {
  const [errorMsg, setErrorMsg] = useState(false);

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
  const formik = useFormik({
    initialValues: { Username: "", Password: "" },
    validate,

    onSubmit: (values) => {
      axios
        .post(
          "/",
          JSON.stringify({
            action: "register",
            username: values.Username,
            password: values.Password,
          })
        )
        .then(function (response) {
          console.log(response);

          /* The response contains: status and a payload data: server/token */
          switch (response.status) {
            /* Register accepted */

            case 200: {
              /* TODO history.push isnt declarative... maybe change this when we have login authentication. */
              Notifications.registerSuccess();
              container.classList.remove("right-panel-active");
              break;
            }
            case 404: {
              Notifications.accountExistsFailure();
              console.log("Error: " + response);
              break;
            }
            default:
              Notifications.unknownFailure();
              break;
          }
        })
        .catch(function (error) {
          console.log(error);
          setErrorMsg(true);
        });
    },
  });
  return (
    <form
      className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
      onSubmit={formik.handleSubmit}
    >
      <p className=" justify-center text-4xl font-bold font-mono">Sign up</p>
      <p className="font-mono text-xl ">
        Sign up to access our amazing community!
      </p>
      <div>
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
          {formik.touched.Password && formik.errors.Password ? (
            <div className="text-red-600">{formik.errors.Username}</div>
          ) : null}
        </div>
        <div>
          {errorMsg ? (
            <label className="text-red-600">
              Register failed! Try something else.
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
          Sign up
        </button>
      </div>
    </form>
  );
};

export default Register;
