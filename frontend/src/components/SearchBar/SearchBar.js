import React from "react";
import "../../assets/main.css";
import InputBox from "./InputBox";

/**
 * A search bar component which will be able to search on given imput through a list
 *
 * @property id associate label with id and add id to input
 * @property value start value of input field
 * @property onButtonClick callback function that gets called when pressing the button
 * @property onInputChange callback function that gets called each time that input field value changes
 * @returns A div containing a form with an input and corresponding lable and button
 */
const SearchBar = ({id, value, onButtonClick, onInputChange}) => {
  return (
    <div className="mt-10">
      <InputBox
        inputStyles = "w-screen50 shadow appearance-none border rounded-full py-2 px-3 focus:outline-none "
        id={id}
        type="text"
        placeholder="Search for..."
        value={value}
        buttonName=""
        onButtonClick={onButtonClick}
        onInputChange={onInputChange}
      />
    </div>
  );
};

export default SearchBar;
