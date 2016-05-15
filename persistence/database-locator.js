'use strict';
var loki = require('lokijs');

class DatabaseLocator {
    constructor() {
        this._databaseName = 'Sheepshead Game Server State';
        this._instance = new loki(this._databaseName);
    }

    get instance() {
        return this._instance;
    }
}

module.exports.DatabaseLocator = DatabaseLocator;
