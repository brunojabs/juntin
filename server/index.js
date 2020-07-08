const options = { origins: '*:*'}
const io = require('socket.io')(options);

io.on('connection', function(socket) {
  socket.on('join', function(roomID){
    socket.join(roomID, () => {
      let rooms = Object.keys(socket.rooms);

      socket.emit('joinedRoom', io.sockets.adapter.rooms[roomID].length);

      console.log(rooms);
    });
  });

  socket.on('sendData', function (data) {
    socket.broadcast.to(data.roomID).emit('syncData', data);
  });
});

io.listen(3000);
