import React from "react";
import { useFormik } from "formik";
import * as actions from "../../actions/actions";
import { useDispatch } from "react-redux";

const axios = require("axios");

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
 * Register provides the layout and the registration for an user account
 * A successfully validated form will redirect to login page pathname = "/"
 * @property history is passed on due to Register being an immediate child of a Route component in Container component
 * @returns a div containing the form to fill out and its validation
 */
const Register = ({ history }) => {
 
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
            /* Login accepted */

            case 200: {
              const data = response.data;
              /* Data should contain token & server */
              console.log(values.Username, values.Password);
              
                /* TODO history.push isnt declarative... maybe change this when we have login authentication. */
              history.push("/");
              break;
            }
            case 404: {
              console.log("Error: "+ response);          
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
    
    },
  });
  return (
    <div className="flex items-center justify-center h-screen">
      <form
        className="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
        onSubmit={formik.handleSubmit}
      >
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
          <div>{formik.touched.Password && formik.errors.Password ? (
            <div className="text-red-600">{formik.errors.Username}</div>
          ) : null}</div>
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
          <div>{formik.touched.Password && formik.errors.Password ? (
            <div className="text-red-600">{formik.errors.Password}</div>
          ) : null}</div>
          <button
            className="5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
            type="submit"
          >
            Register
          </button>
        </div>
      </form>
    </div>
  );
};

export default Register;
