var User = require('./model').User,
    loki = require('lokijs'),
    winston = require('winston'),
    collection = require('lodash/collection'),
    util = require('./utility-functions'),

    db = new loki('a.json'),
    users = db.addCollection('users');

module.exports.registerUser = function(userToAdd) {
    //TODO: add check for existing nickname
    userToAdd.ticket = generateTicket(userToAdd);
    users.insert(userToAdd);

    return new User(userToAdd);
};

module.exports.findUserByNickname = function(nickname) {
    if (util.isVoid(nickname)) {
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
};

function generateTicket(userToAdd) {
    var isUnique = false,
        currentTicket = "";

    while(!isUnique) {
        currentTicket = generateTicketString(); 
        var userWithTicket = findUserWithTicket(currentTicket);
        isUnique = util.isVoid(userWithTicket);
    }

    return currentTicket;
}

function generateTicketString() {
    var dictionary = ['a','b','c','d','e','f','g','h','i','j',
        'k','l','m','n','o','p','q','r','s','t','u','v','w',
        'x','y','z','0','1','2','3','4','5','6','7','8','9'];
    var ticketLength = 32;

    return collection.sample(dictionary, ticketLength).join('');
}

function findUserWithTicket(ticket) {
    var databaseResults = users.chain()
        .find({ticket: { '$eq': ticket }})
        .data();

    if (databaseResults.length) {
        return databaseResults[0];
    }
}
