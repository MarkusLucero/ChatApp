import React from "react";

/**
 * Input field form component 
 *
 * @property id associate label with id and add id to input
 * @property inputStyles className will have this line of string
 * @property type type of input field
 * @property placeholder placeholder for the input field
 * @property value start value of input field
 * @property buttonName text on the button
 * @property onButtonClick callback function that gets called when pressing the button
 * @property onInputChange callback function that gets called each time that input field value changes
 * @returns an input field form with a corresponding button
 */
const InputBox = ({
  id,
  inputStyles,
  type,
  placeholder,
  value,
  buttonName,
  onButtonClick,
  onInputChange,
}) => {
  return (

    <form className="flex justify-center focused-view-custom-bg">
      <input
        className={inputStyles}
        id={id}
        type={type}
        value={value}
        placeholder={placeholder}
        onChange={onInputChange}
      />
      <button onClick={onButtonClick}>{buttonName}</button>
    </form>
  );
};

export default InputBox;
