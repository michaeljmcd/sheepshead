var Room = require('./room').Room,
    winston = require('winston'),
    l_ = require('lodash'),
    database = require('../persistence/database');

function getGameRooms() {
    var allRooms = database.getRoomCollection.find({});

    return l_.map(allRooms, function(val) {
        return new Room(val);
    });
}

function registerGameRoom (gameRoom) {
}

module.exports.getGameRooms = getGameRooms;
module.exports.registerGameRoom = registerGameRoom;
