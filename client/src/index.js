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
  console.log('sendData', message);
  socket.emit('sendData', message);
});

app.ports.joinRoom.subscribe(function (roomID) {
  socket.emit('join', roomID)
});

socket.on('syncData', function(msg){
  console.log('received', msg);
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

      break;
    default:
      break;
  }
});

const stateNames = {
  '-1': 'Unstarted',
  0: 'Ended',
  1: 'Playing',
  2: 'Paused',
  3: 'Buffering',
  5: 'VideoCued'
};

player.on('stateChange', ({ data }) => {
  app.ports.playerMsgReceiver.send(stateNames[data] || "");
});
