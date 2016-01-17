'use strict';
var util = require('./utility-functions');

function User() {
    this.nickname = "";
    this.ticket = "";

    if (arguments && arguments.length === 1 && !util.isVoid(arguments[0])) {
        var objectToCopy = arguments[0];

        this.nickname = objectToCopy.nickname || this.nickname;
        this.ticket = objectToCopy.ticket || this.ticket;
    }
}

User.prototype.isValid = function() {
    var me = this;
    return !!(me.nickname && me.nickname.length <= 255);
};

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
};

Room.prototype.isValid = function() {
    var isValid = true,
        me = this;

    isValid = isValid && (typeof me.seats === 'number' && me.seats >= 3 && me.seats <= 5);

    return isValid;
};

module.exports.User = User;
module.exports.Room = Room;
