import './reset.css';
import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';
import io from 'socket.io-client';
import YouTubePlayer from 'youtube-player';
import Plyr from 'plyr';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();


var socket = io(process.env.ELM_APP_SOCKET_ADDRESS);

app.ports.sendData.subscribe(function(message) {
  socket.emit('sendData', message);
});

app.ports.joinRoom.subscribe(function (roomID) {
  socket.emit('join', roomID)
});

socket.on('syncData', function(msg){
  app.ports.dataReceiver.send(msg);
});

socket.on('joinedRoom', function(data) {
  app.ports.joinedRoom.send(data);
});

let player = null;
let plyr = null;


const observer = new MutationObserver((mutationList, observer) => {
  if( anyPlayer(mutationList) ) {
      //player = YouTubePlayer('yt-player');
      //player.loadVideoById('M7lc1UVf-VE').then( () => {
      //});

        window.plyr = new Plyr('#player', { 'youtube': { 'modestbranding': 1, 'rel': 1, 'controls': 0 }} );
      //player.on('stateChange', ({ data }) => {
        //app.ports.playerMsgReceiver.send(stateNames[data] || "");
      //});
  }
});

const anyPlayer = (mutationList) => {
  return mutationList.some(mutation => {
    return Array.from(mutation.addedNodes).some(node => {
      return node.className === "player__wrapper";
    })
  })
}
const targetNode = document.querySelector('.content__wrapper');
observer.observe(targetNode, {subtree: true , childList: true });

app.ports.emitPlayerMsg.subscribe( ({message, data}) =>  {

  switch(message) {
    case 'play':
        player.playVideo();

      break;
    case 'pause':
        player.pauseVideo()

      break;
    case 'loadVideo':
      // NOTE: LoadVideo loads the video and play it as soon as it is loaded
      player.loadVideoById(data.videoID, data.time);

      break;
    case 'cueVideo':
      // NOTE: CueVideo loads the video but don't start to play it
      player.cueVideoById(data.videoID, data.time)

      break;
    case 'getCurrentTime':
      player.getCurrentTime().then(function(time) {
        app.ports.playerCurrentTimeReceiver.send(time || 0.0);
      });

      break;
    case 'loadPlayer':
      //player = YouTubePlayer('yt-player', { playerVars: { 'autoplay': 0, 'controls': 0 }}).then(() => { ;
      //plyr = new Plyr('#player') })
      //player.on('stateChange', ({ data }) => {
        //app.ports.playerMsgReceiver.send(stateNames[data] || "");
      //});

    //window.plyr = plyr;

    //plyr.source = {
      //type: 'video',
      //sources: [
        //{
          //src: 'bTqVqk7FSmY',
          //provider: 'youtube',
        //},
      //],
    //}

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

