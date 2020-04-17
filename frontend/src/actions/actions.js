export function connect(ws) {
    return { type: "CONNECT", payload: ws };
  }  

export function login(values) {
  return { type: "LOGIN", payload: values };
}


export function response(data) {
  return { type: "RESPONSE", payload: data};
}

