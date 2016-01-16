var User = require('./lib/model').User,
    userLogic = require('./lib/user-functions'),
    roomLogic = require('./lib/room-functions');

module.exports.index = function* () {
    this.body = 'hello world';
};

module.exports.connectUser = function* () {
    var user = new User();
    this.body = user;
};

module.exports.getPublicGameRooms = function* () {
};
