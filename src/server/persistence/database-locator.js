'use strict';
var loki = require('lokijs');

let instance = null;

class DatabaseLocator {
    constructor() {
        this.databaseName = 'Sheepshead Game Server State';

        if (!instance) {
            instance = new loki(this.databaseName);
        }
    }

    get instance() {
        return instance;
    }
}

module.exports.DatabaseLocator = DatabaseLocator;
