var Room = require('./room').Room,
    winston = require('winston'),
    loki = require('lokijs'),
    l_ = require('lodash'),
    DatabaseLocator = require('../persistence/database-locator').DatabaseLocator,
    
    db = new DatabaseLocator().instance,
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
