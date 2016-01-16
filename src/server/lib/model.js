/* jshint esnext:true  */

module.exports.User = function() {
    this.nickname = "";
    this.ticket = "";

    if (arguments && arguments.length === 1) {
        var objectToCopy = arguments[0];
        this.nickname = objectToCopy.nickname;
        this.ticket = objectToCopy.ticket;
    }
};

module.exports.Room = function() {
    this.id = "";
    this.name = "";
    this.seats = 3;
    this.availableSeats = 3;
};
