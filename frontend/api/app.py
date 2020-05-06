from concurrent import futures
import json
import time

from flask import Flask, jsonify, request, render_template
from flask_cors import CORS


app = Flask(__name__)
CORS(app)

"""
    PROOF OF CONCEPT
"""

""" hard coded jsonfiles used to search through """
files = ["c1.json", "c2.json", "c3.json", "c4.json", "c5.json",
         "c6.json", "c7.json", "c8.json", "c9.json", "c10.json", ]

search_term = ""  # global variabel for search term

""" on fetch to localhost:5000/search this func will trigger """
@app.route('/search', methods=['POST'])
def search():

    # POST request
    if request.method == 'POST':
        print('Incoming POST..')
        data = request.get_json(force=True)  # turn the object received to JSON
        print("search term is: " + format(data["search_term"]))

        """ trigger func that will handle the concurrent search """
        result = handle_search(data["search_term"])

        """ trigger func that will handle the "normal" search """
        #result = handle_search_normal(data["search_term"])

        return jsonify({'OK': 200, 'data': result})


""" ignore """
@app.route('/test')
def test_page():
    # look inside `templates` and serve `index.html`
    return render_template('index.html')


def search_file(n):
    jsonfile = open(f"./{files[n]}")
    data = json.load(jsonfile)
    messages_in_file = []
    messages = []
    chat_name = data["chatName"]
    chat = data["messages"]
    for m in chat:
        message = m["message"]
        if search_term in message:
            messages.append(
                {"message": message, "username": m["username"]})
    messages_in_file.append(
        {"chatName": chat_name, "searchedMessages": messages})

    return messages_in_file


def handle_search(term):
    start = time.time()
    global search_term  # get the global variabel call search_term
    search_term=term  # change the global variable

    """ uncomment the one you want to use (thread/process)"""
    #ex=futures.ThreadPoolExecutor() ## thread based concurrency
    ex = futures.ProcessPoolExecutor()  # process based (parallelism)
    results = ex.map(search_file, range(len(files)-1, 0, -1))
    real_results = list(results)
    end = time.time()
    print(f"\nTime to complete: {end -start:.2f}s\n")
    return real_results


""" uncoment below and coment above to use search without thread based concurrency """


def handle_search_normal(term):
    start = time.time()
    global search_term  # get the global variabel call search_term
    search_term=term  # change the global variable

    real_results = []

    for file in files:
        jsonfile = open(f"./{file}")
        data = json.load(jsonfile)
        messages = []
        chat_name = data["chatName"]
        chat = data["messages"]
        for m in chat:
            message = m["message"]
            if search_term in message:
                messages.append(
                    {"message": message, "username": m["username"]})
        real_results.append(
            {"chatName": chat_name, "searchedMessages": messages})
    end = time.time()
    print(f"\nTime to complete: {end -start:.2f}s\n")
    return real_results
