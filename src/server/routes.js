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
    winston.info("User %s registered with ticket %s", createdUser.nickname, createdUser.ticket);

    this.body = createdUser;
};

module.exports.getPublicGameRooms = function* () {
    this.body = roomRepository.getGameRooms();
    yield this.body;
};
