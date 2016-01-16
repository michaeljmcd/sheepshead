/* jshint esnext:true  */
var util = require('./utility-functions');

module.exports.User = function() {
    this.nickname = "";
    this.ticket = "";

    if (arguments && arguments.length === 1 && !util.isVoid(arguments[0])) {
        var objectToCopy = arguments[0];
        this.nickname = objectToCopy.nickname;
        this.ticket = objectToCopy.ticket;
    }
};

module.exports.User.prototype.isValid = function() {
    var me = this;
    return  me.nickname && me.nickname.length <= 255;
};

module.exports.Room = function() {
    this.id = "";
    this.name = "";
    this.seats = 3;
    this.availableSeats = 3;
};
