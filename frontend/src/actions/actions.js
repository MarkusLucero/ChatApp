export function connect(url) {
    return { type: "CONNECTED", payload: url };
  }  

export function login(values) {
  return { type: "LOGIN", payload: values };
}

