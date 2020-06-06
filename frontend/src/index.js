import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import ReconnectingWebSocket from 'reconnecting-websocket';


const app = Elm.Main.init({
  node: document.getElementById('root')
});


const socket = new ReconnectingWebSocket('ws://localhost:7000/api');

socket.addEventListener('open', function (event) {
  console.log('Websocket connection opened', event.data);
  app.ports.socketConnect.send(null);
});

socket.addEventListener('close', function (event) {
  console.log('Websocket connection closed', event.data);
  app.ports.socketDisconnect.send(null);
});

socket.addEventListener('error', function (event) {
  console.log('Websocket connection error', event.data);
  app.ports.socketDisconnect.send(null);
});

socket.addEventListener('message', function (event) {
  console.log('Message from server ', event.data);
  app.ports.receiveMessage.send(event.data);
});

app.ports.sendMessage.subscribe(function (messageData) {
  console.log('Sending message ', messageData);
  socket.send(JSON.stringify(messageData));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
