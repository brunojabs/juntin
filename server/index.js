const options = {
  cors: {
    origin: "*",
  },
};

const io = require("socket.io")(options);

const port = process.env.PORT || 3001;

io.on("connection", function (socket) {
  socket.on("join", function (roomID) {
    socket.join(roomID);

    io.to(roomID).emit("joinedRoom", io.of("/").adapter.rooms.get(roomID).size);
  });

  socket.on("sendData", function (data) {
    socket.to(data.roomID).emit("syncData", data);
  });
});

io.listen(port);
