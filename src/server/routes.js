var User = require('./lib/model').User;

module.exports.index = function* () {
    this.body = 'hello world';
};

module.exports.connectUser = function* () {
    var user = new User();
    this.body = user;
};

