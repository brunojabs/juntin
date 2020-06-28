const options = { origins: '*:*'}
const io = require('socket.io')(options);

io.on('connection', function(socket) {
  socket.on('join', function(roomID){
    socket.join(roomID, () => {
      let rooms = Object.keys(socket.rooms);

      socket.broadcast.to(roomID).emit('connection');
      console.log(rooms);
    });
  });

  socket.on('sendData', function (data) {
    console.log(data)
    socket.broadcast.to(data.roomID).emit('syncData', data );
  });
});

io.listen(3000);
