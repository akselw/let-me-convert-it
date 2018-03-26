const express = require('express');

const server = express();

server.use(express.static('./build'));
server.listen(8080, () => console.log('Express app listening on port 8080'));
