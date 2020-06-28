import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import io from 'socket.io-client';
import YouTubePlayer from 'youtube-player';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


var socket = io('localhost:3000');

app.ports.sendData.subscribe(function(message) {
  socket.emit('sendData', message);
});

app.ports.joinRoom.subscribe(function (roomID) {
  socket.emit('join', roomID)
});

socket.on('syncData', function(msg){
  app.ports.dataReceiver.send(msg);
});


let player;

player = YouTubePlayer('player');
player.loadVideoById('hBCUuSr-0Nk', 150);

app.ports.emitPlayerMsg.subscribe(function(message) {
  switch(message) {
    case 'play':
      player.playVideo();

      break;
    case 'pause':
      console.log('pause');
      player.pauseVideo()

      break;
    default:
      // code block
  }
});
