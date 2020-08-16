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

app.ports.initPlayer.subscribe(() => initObserver());

const subscribeToMsgs = (player) => {
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
      default:
        break;
    }
  })
}

const initObserver = () => {
    const observer = new MutationObserver((mutationList, observer) => {
      if( anyPlayer(mutationList) ) {
        observer.disconnect();
        subscribeToMsgs(new Plyr('#player'));
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
}

