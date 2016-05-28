var MongoClient = require('mongodb').MongoClient,
    DATABASE_URI = 'mongodb://localhost:27017/sheepshead',
    database;

exports.connect = function(callback) {
    MongoClient.connect(DATABASE_URI, function(err, db) {
        if (err) {
            throw err;
        }

        database = db;
        
        if (callback) {
            callback();
        }
    });
};

// TODO: figure out when we close the database
exports.disconnect = function(callback) {
    database.close(true, callback);
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

exports.clearUsers = function() {
    exports.getUserCollection().remove({});
};

exports.clearRooms = function() {
    exports.getRoomCollection().remove({});
};
