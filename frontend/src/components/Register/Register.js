import React from "react";
import { useFormik } from "formik";

const Connection = () => {
  let ws = new WebSocket("ws://localhost:3000/ws");
};

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
      /* TODO history.push isnt declarative... maybe change this when we have login authentication. */
      history.push("/");
    },
  });
  return (
    <div class="flex-auto w-full max-w-xs">
      <form
        class="flex-auto bg-white shadow-md rounded px-8 pt-6 pb-8 mb-4 "
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
            value={formik.values.Username}
          ></input>
          {formik.errors.Username ? (
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
            value={formik.values.Password}
          ></input>
          {formik.errors.Password ? (
            <div class="text-red-600">{formik.errors.Password}</div>
          ) : null}
          <button
            class="5pxbg-white hover:bg-gray-100 text-gray-800 font-semibold py-2 px-4 border border-gray-400 rounded shadow"
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
