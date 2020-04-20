import React from "react";
import { Link } from "react-router-dom";
import { useFormik } from "formik";
import * as actions from "../../actions/actions";
import { useDispatch } from "react-redux";

const axios = require("axios");

//axios.get("http://localhost:8080/", { crossdomain: true })
const validate = (values) => {
  const errors = {};
  if (!values.Username) {
    errors.Username = "Required";
  }

  if (!values.Password) {
    errors.Password = "Required";
    return errors;
  }
};

/**
 * Login provides the layout and login of a user
 * A successfully validated form will redirect to LandingPage with pathname = "/start"
 * @property history is passed on due to Login being an immediate child of a Route component in Container component
 * @returns a div containing the form to fill out and its validation
 */
const Login = ({ history }) => {
  /* useDispatch from dispatch function from store */
  const dispatch = useDispatch();

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
            Password: values.Password,
          })
        )
        .then(function (response) {
          console.log(response);
          
          /* The response contains: status and a payload data: server/token */
          switch (response.status) {
            /* Login accepted */

            case 200: {
              const data = response.data;

              /* Data should contain token & server */
              dispatch(actions.setServer( "ws://localhost:8080/websocket" ));
              dispatch(actions.loginSuccess({ data }));
              //dispatch(actions.setServer({ data }));
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
          console.log(response);
        })
        .catch(function (error) {
          console.log(error);
        });
      /* Use the store reducer to dispatch login actions */
      dispatch(actions.login({ values }));
      /* TODO history.push isnt declarative... maybe change this when we have login authentication. */
      history.push("/start");
    },
  });
  return (
    <div
      style={{
        backgroundImage: "url(" + require("../../background_night.png") + ")",
      }}
      className="flex items-center justify-center h-screen bg-scroll"
    >
      <form
        className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
        onSubmit={formik.handleSubmit}
      >
        <p className=" justify-center">Welcome!</p>
        <p>Enter your login details to access Chat Up!</p>

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
          <button
            className="5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
            type="submit"
          >
            Login
          </button>
          <Link
            to="/register"
            className=" mt-10 ml-40 5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
            type="button"
          >
            Register
          </Link>
        </div>
      </form>
    </div>
  );
};

export default Login;
