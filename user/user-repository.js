var User = require('./user').User,
    winston = require('winston'),
    collection = require('lodash/collection'),
    util = require('../util/utility-functions'),
    database = require('../persistence/database');

function registerUser (input) {
    var userToAdd = new User(input),
        users = database.getUserCollection();

    userToAdd.ticket = generateTicket(userToAdd);
    assignUniqueNickname(userToAdd);

    users.insert(userToAdd);

    return new User(userToAdd);
}

function findUserByNickname (nickname) {
    var users = database.getUserCollection();

    if (util.isVoid(nickname)) {
        return null;
    }

    if (nickname === '') {
        return null;
    }

    var databaseResults = users.find({nickname: { '$eq': nickname }});

    if (databaseResults.length) {
        return databaseResults[0];
    }
}

module.exports.registerUser = registerUser;
module.exports.findUserByNickname = findUserByNickname;

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

function assignUniqueNickname(user) {
    var userAlreadyExists = !!findUserByNickname(user.nickname),
        incrementCount = 1,
        newNickname = user.nickname;

    while(userAlreadyExists) {
        newNickname = user.nickname + incrementCount++;
        userAlreadyExists = !!findUserByNickname(newNickname);
    }

    user.nickname = newNickname;
}

function generateTicketString() {
    var dictionary = ['a','b','c','d','e','f','g','h','i','j',
        'k','l','m','n','o','p','q','r','s','t','u','v','w',
        'x','y','z','0','1','2','3','4','5','6','7','8','9'];
    var ticketLength = 32;

    return collection.sample(dictionary, ticketLength).join('');
}

function findUserWithTicket(ticket) {
    var users = database.getUserCollection(),
        databaseResults = users.find({ticket: { '$eq': ticket }});

    if (databaseResults.length) {
        return databaseResults[0];
    }
}
