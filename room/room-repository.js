var Room = require('./room').Room,
    winston = require('winston'),
    l_ = require('lodash'),
    database = require('../persistence/database');

function getGameRooms() {
    var allRooms =  database.getRoomCollection().find().toArray();
    return l_.map(allRooms, function(val) {
        return new Room(val);
    });
}

function registerGameRoom (gameRoom) {
    return new Promise(function(resolve, reject) {
        var roomToAdd = new Room(gameRoom),
            rooms = database.getRoomCollection();

        try {
            rooms.insert(roomToAdd, null, function(err, result) {
               resolve(roomToAdd);
            });
        }
        catch(err) {
            reject(err);
        }
    });
}

module.exports.getGameRooms = getGameRooms;
module.exports.registerGameRoom = registerGameRoom;
