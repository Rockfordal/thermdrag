var port = "4500";

var http = require('http');

// Create a http server with a callback handling all requests
var httpServer = http.createServer(function(request, response) {
  console.log((new Date()) + ' Received request for ' + request.url);
  response.writeHead(200, {'Content-type': 'text/plain'});
  response.end('Hello world\n');
});

// Setup the http-server to listen to a port
httpServer.listen(port, function() {
  console.log((new Date()) + ' HTTP server is listening on port ' + port);
});

var WebSocketServer = require('websocket').server;

wsServer = new WebSocketServer({
  httpServer: httpServer,
  autoAcceptConnections: false
});

// Create a callback to handle each connection request
wsServer.on('request', function(request) {
  var connection = request.accept();
  console.log((new Date()) + ' Connection accepted from ' + request.origin);

  // Callback to handle each message from the client
  connection.on('message', function(message) {
      if (message.type === 'utf8') {
          console.log('Received Message: ' + message.utf8Data);
          connection.sendUTF(message.utf8Data);
      }
      else if (message.type === 'binary') {
          console.log('Received Binary Message of ' + message.binaryData.length + ' bytes');
          connection.sendBytes(message.binaryData);
      }
  });
  
  // Callback when client closes the connection
  connection.on('close', function(reasonCode, description) {
      console.log((new Date()) + ' Peer ' + connection.remoteAddress + ' disconnected.');
  });
});