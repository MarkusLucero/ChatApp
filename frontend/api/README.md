
    /* OBS OBS !! IF YOU WANT TO TRY OUT THE PYTHON SEARCH API USE THE FETCH STATEMENT BELOW IN handleSearchSubmit */
    // Send the same request
    
    fetch("http://localhost:5000/search", {
      // Specify the method
      method: "POST",
      // A JSON payload
      body: JSON.stringify({
        "search_term": searchTerm,
      }),
    })
      .then(function (response) {
        return response.json(); //parse result as JSON
      })
      .then(function (json) {
        console.log("Search results: ");
        console.log(json); // Here’s our JSON object
      });