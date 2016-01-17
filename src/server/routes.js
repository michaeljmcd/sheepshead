'use strict';

var User = require('./lib/model').User,
    userLogic = require('./lib/user-functions'),
    roomLogic = require('./lib/room-functions'),
    util = require('./lib/utility-functions'),

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
    createdUser = userLogic.registerUser(newUser); 
    winston.info("User %s registered with ticket %s", createdUser.nickname, createdUser.ticket);

    this.body = createdUser;
};

module.exports.getPublicGameRooms = function* () {
    this.body = roomLogic.getGameRooms();
    yield this.body;
};
