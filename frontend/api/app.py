from concurrent import futures
import json
import threading
import time
from flask import Flask, jsonify, request, render_template
from flask_cors import CORS

app = Flask(__name__)
CORS(app)

""" 
    PROOF OF CONCEPT 
"""

""" hard coded jsonfile used to search through """
jsonfile = open("./test.json")
data = json.load(jsonfile)
search_term = "" ## global variabel for search term 

""" on fetch to localhost:5000/search this func will trigger """
@app.route('/search', methods=['POST'])
def hello():

    # POST request
    if request.method == 'POST':
        print('Incoming POST..')
        data = request.get_json(force=True) # turn the object received to JSON
        print("search term is: " + format(data["search_term"]))
        
        result = handle_search(data["search_term"]) # trigger func that will handle the concurrent search
        return jsonify({'OK': 200, 'data': result})

""" ignore """
@app.route('/test')
def test_page():
    # look inside `templates` and serve `index.html`
    return render_template('index.html')


""" 
    the function that we call each worker todo 
    n represents index of data array -> (test.json)
 """
def task(n):
    print('{}: sleeping on chat {}'.format(
        threading.current_thread().name,
        n)
    )

    chat_obj = data[n]
    chat_name = chat_obj["chatName"]

    chat = chat_obj["messages"]
    messages = []
    for m in chat:
        message = m["message"]
        if search_term in message:
            messages.append(
                {"message": message, "username": m["username"]})

    print('{}: done with {}'.format(
        threading.current_thread().name,
        n)
    )
    
    return {"chatName": chat_name, "searchedMessages": messages}


def handle_search(term):

    global search_term # get the global variabel call search_term
    search_term = term # change the global variable 


    ex = futures.ThreadPoolExecutor(max_workers=len(data)) # construct workers x length of data array
    print('main: starting')
    print("searching for : '{}'".format(search_term))
    results = ex.map(task, range(len(data)-1, 0, -1)) # map the task func with lenght of data array as arg to each worker

    real_results = list(results) 
    print('main: {}'.format(real_results))
    return real_results
