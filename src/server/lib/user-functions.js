var User = require('./model').User,
    loki = require('lokijs'),
    winston = require('winston'),
    _ = require('lodash'),

    db = new loki('a.json'),
    users = db.addCollection('users');

function isVoid(value) {
    return value === null || value === undefined;
}

function generateTicket(userToAdd) {
    return 1; // TODO: finishme
}

module.exports.registerUser = function(userToAdd) {
    userToAdd.ticket = generateTicket(userToAdd);
    users.insert(userToAdd);
};

module.exports.findUserByNickname = function(nickname) {
    if (isVoid(nickname)) {
        return null;
    }

    if (nickname === '') {
        return null;
    }

    var databaseResults = users.chain()
        .find({nickname: { '$eq': nickname }})
        .data();

    if (databaseResults.length) {
        return databaseResults[0];
    }
}
