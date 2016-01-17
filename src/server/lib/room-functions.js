var Room = require('./model').Room,
    winston = require('winston'),
    loki = require('lokijs'),
    
    db = new loki('a.json'),
    gameRoomDb = db.addCollection('gameRooms');

module.exports.getGameRooms = function() {
};
