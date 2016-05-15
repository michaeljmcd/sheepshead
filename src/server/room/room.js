'use strict';
var util = require('../util/utility-functions');

function Room() {
    this.id = "";
    this.name = "";
    this.seats = 3;
    this.availableSeats = 3;

    if (arguments && arguments.length === 1 && !util.isVoid(arguments[0])) {
        var objectToCopy = arguments[0];

        this.id = objectToCopy.id;
        this.name = objectToCopy.name;
        this.seats = objectToCopy.seats;
        this.availableSeats = objectToCopy.availableSeats;
    }
}

Room.prototype.isValid = function() {
    var isValid = true,
        me = this;

    isValid = isValid && (typeof me.seats === 'number' && me.seats >= 3 && me.seats <= 5);

    return isValid;
};

module.exports.Room = Room;
