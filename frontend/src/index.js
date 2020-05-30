import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


const socket = new WebSocket('ws://localhost:7000/api');

const app = Elm.Main.init({
  node: document.getElementById('root')
});

socket.addEventListener('message', function (event) {
  console.log('Message from server ', event.data);
  app.ports.receiveMessage.send(event.data);
});

app.ports.sendMessage.subscribe(function (messageData) {
  console.log('Sending message ', messageData);
  socket.send(messageData);
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
