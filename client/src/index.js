import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import io from 'socket.io-client';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

window.app = app;


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


var socket = io('http://24dabcb59ea5.ngrok.io');

app.ports.sendData.subscribe(function(message) {
  socket.emit('sendData', message);
});

app.ports.joinRoom.subscribe(function (roomID) {
  socket.emit('join', roomID)
});

socket.on('syncData', function(msg){
  app.ports.dataReceiver.send(msg);
});
