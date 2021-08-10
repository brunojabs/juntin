import "./reset.css";
import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";
import { io } from "socket.io-client";
import YouTubePlayer from "youtube-player";

const app = Elm.Main.init({
  node: document.getElementById("root"),
  flags: { youtubeAPIKey: process.env.ELM_APP_YOUTUBE_API_KEY },
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();

const socket = io(process.env.ELM_APP_SOCKET_ADDRESS, {});

app.ports.sendData.subscribe(function (message) {
  socket.emit("sendData", message);
});

app.ports.joinRoom.subscribe(function (roomID) {
  socket.emit("join", roomID);
});

socket.on("syncData", function (msg) {
  app.ports.dataReceiver.send(msg);
});

socket.on("joinedRoom", function (data) {
  app.ports.joinedRoom.send(data);
});

let player = null;

app.ports.emitPlayerMsg.subscribe(({ message, data }) => {
  if (!player) {
    player = YouTubePlayer("player", {
      playerVars: { autoplay: 0, controls: 0 },
    });
    player.on("stateChange", ({ data }) => {
      app.ports.playerMsgReceiver.send(stateNames[data] || "");
    });
  }

  switch (message) {
    case "play":
      player.playVideo();

      break;
    case "pause":
      player.pauseVideo();

      break;
    case "loadVideo":
      // NOTE: LoadVideo loads the video and play it as soon as it is loaded
      player.loadVideoById(data.videoID, data.time);

      break;
    case "cueVideo":
      // NOTE: CueVideo loads the video but don't start to play it
      player.cueVideoById(data.videoID, data.time);

      break;
    case "getCurrentTime":
      player.getCurrentTime().then(function (time) {
        app.ports.playerCurrentTimeReceiver.send(time || 0.0);
      });

      break;
    case "setVolume":
      player.setVolume(data.value);

      break;
    default:
      break;
  }
});

const stateNames = {
  "-1": "Unstarted",
  0: "Ended",
  1: "Playing",
  2: "Paused",
  3: "Buffering",
  5: "VideoCued",
};
