'use strict';

var User = require('./user/user').User,
    userRepository = require('./user/user-repository'),
    roomRepository = require('./room/room-repository'),
    util = require('./util/utility-functions'),

    parse = require('co-body'),
    winston = require('winston');

module.exports.connectUser = function* () {
    var parsedBody = yield parse.json(this),
        newUser = new User(parsedBody),
        createdUser = null;

    if (util.isVoid(parsedBody) || !newUser.isValid()) {
        this.throw('Invalid user received', 422);
    }

    winston.info("Attempting to register user", newUser);
    createdUser = userRepository.registerUser(newUser); 

    this.body = yield createdUser;
};

module.exports.getPublicGameRooms = function* () {
    var result = roomRepository.getGameRooms();
    this.body = result;
    yield this.body;
};

module.exports.createPublicGameRoom = function* () {
};
