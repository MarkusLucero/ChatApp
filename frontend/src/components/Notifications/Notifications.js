import { toast } from "react-toastify";
import "react-toastify/dist/ReactToastify.css";


/**
 * Contains notifications to indicate success or failure upon registration
 * 
 */


toast.configure();
export function registerSuccess() {
  toast.success("Your account was successfully created!", {
    position: toast.POSITION.TOP_CENTER,
    
    autoClose: 2500
  });
}
export function accountExistsFailure() {
  toast.error("That account already exists!", {
    position: toast.POSITION.TOP_CENTER,
    autoClose: 2500
  });
}
export function unknownFailure() {
    toast.error("That account already exists!", {
      position: toast.POSITION.TOP_CENTER,
      autoclose: 2500
    });
}
