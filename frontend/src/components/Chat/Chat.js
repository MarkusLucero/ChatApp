import React, {useState} from 'react';
import "../../assets/main.css";

/**
 * Chat holds the layout of all messages and the input/sent
 * 
 * @returns a div containing all messages aswell as input for new messages
 */
const Chat = () => {

    //All old messages
    const [messages, setMessages] = useState(["Hej", "Då"]);
    //When typing a new message
    const [newMessage, setNewMessage] = useState(''); 

    const sendMessage = (e) =>{
        e.preventDefault(); 
        setMessages(prev => [...prev, newMessage]); 
        setNewMessage('');
        };

    
    return (
        <div className ="bg-gray-800 container mx-auto h-screen" >
            <div id="messages">
                {messages.map((message, index) => (
                    <div key ={index} className="static bg-gray-400 display-flex">           
                        {message}
                    </div>            
                ))}
            </div>
            <div id="input&send"className="static w-full">
                <form onSubmit={sendMessage}>
                    <input className="bg-blue-200 w-5/6"type="text" onChange = {e => setNewMessage(e.target.value)} placeholder="New message..."/>
                    <button onClick={sendMessage} className="w-1/6 bg-blue-100"type="submit"> 
                    Send
                    </button>
                </form>
            </div>
        </div>
    )
}


export default Chat;
