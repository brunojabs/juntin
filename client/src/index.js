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

player = YouTubePlayer('player', { playerVars: { 'autoplay': 0 }});

app.ports.emitPlayerMsg.subscribe( ({message, data}) =>  {
  switch(message) {
    case 'play':
        player.playVideo();

      break;
    case 'pause':
        player.pauseVideo()

      break;
    case 'loadVideo':
      player.cueVideoById(data);
    default:
      // code block
  }
});

const stateNames = {
  '-1': 'unstarted',
  0: 'ended',
  1: 'playing',
  2: 'paused',
  3: 'buffering',
  5: 'video cued'
};

player.on('stateChange', ({ data }) => {
  console.log(data);
  app.ports.playerMsgReceiver.send(stateNames[data] || "");
});
