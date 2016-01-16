var User = require('./lib/model').User,
    userLogic = require('./lib/user-functions'),
    roomLogic = require('./lib/room-functions'),
    util = require('./lib/utility-functions'),

    parse = require('co-body'),
    winston = require('winston');

module.exports.index = function* () {
    this.body = 'hello world';
};

module.exports.connectUser = function* () {
    var userToConnect = yield parse.json(this),
        createdUser = null;

    if (util.isVoid(userToConnect) || !userToConnect.nickname || userToConnect.nickname.length > 255) {
        this.throw('Invalid user received', 422);
    }

    winston.info("Attempting to register user %j", userToConnect);
    createdUser = userLogic.registerUser(userToConnect); 
    winston.info("User %s registered with ticket %s", createdUser.nickname, createdUser.ticket);

    this.body = createdUser;
};

module.exports.getPublicGameRooms = function* () {
};
