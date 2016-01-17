var Room = require('./model').Room,
    winston = require('winston'),
    loki = require('lokijs'),
    l_ = require('lodash'),
    
    db = new loki('Sheepshead Game Server State'),
    gameRooms = db.addCollection('gameRooms');

function getGameRooms() {
    return l_.map(gameRooms.data, function(val) {
        return new Room(val);
    });
}

function registerGameRoom (gameRoom) {
}

module.exports.getGameRooms = getGameRooms;
module.exports.registerGameRoom = registerGameRoom;
