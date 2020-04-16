import React, { useState } from "react";
import { Link } from "react-router-dom";
import { useFormik } from "formik";

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
  const [text, setText] = useState("");

  const formik = useFormik({
    initialValues: { Username: "", Password: "" },
    validate,
    onSubmit: (values) => {
      /* TODO history.push isnt declarative... maybe change this when we have login authentication. */
      history.push("/start");
    },
  });
  return (
    <div class="flex items-center justify-center h-screen">
      <form
        class="bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
        onSubmit={formik.handleSubmit}
      >
        <div class="mb-4">
          <label
            class="block text-gray-700 text-sm font-bold mb-2"
            htmlFor="Username"
          >
            Username
          </label>
          <input
            class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            id="Username"
            name="Username"
            type="Text"
            onChange={formik.handleChange}
            onBlur={formik.handleBlur}
            value={formik.values.Username}
          ></input>
          {formik.touched.Username && formik.errors.Username ? (
            <div class="text-red-600">{formik.errors.Username}</div>
          ) : null}
          <label
            class="block text-gray-700 text-sm font-bold mb-2"
            htmlFor="Password"
          >
            Password
          </label>
          <input
            class="shadow appearance-none border rounded w-full py-2 px-3 text-gray-700 leading-tight focus:outline-none focus:shadow-outline"
            id="Password"
            name="Password"
            type="Password"
            onChange={formik.handleChange}
            onBlur={formik.handleBlur}
            value={formik.values.Password}
          ></input>
          {formik.touched.Password && formik.errors.Password ? (
            <div class="text-red-600">{formik.errors.Password}</div>
          ) : null}
          <button
            class="5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
            type="submit"
          >
            Login
          </button>
          <Link
            to="/register"
            class=" mt-10 ml-40 5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
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
