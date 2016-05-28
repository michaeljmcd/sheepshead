var MongoClient = require('mongodb').MongoClient,
    DATABASE_URI = 'mongodb://localhost:27017/sheepshead',
    database;

exports.connect = function(callback) {
    MongoClient.connect(DATABASE_URI, function(err, db) {
        if (err) {
            throw err;
        }

        database = db;
        callback();
        // TODO: figure out how we close the database
    });
};

exports.getDatabase = function() {
    return database;
};

exports.getUserCollection = function() {
    return database.collection('users');
};

exports.getRoomCollection = function() {
    return database.collection('rooms');
}
