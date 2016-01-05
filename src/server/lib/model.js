/* jshint esnext:true  */

module.exports.User = function() {
    this.nickname = "";
    this.ticket = "";
};

module.exports.Room = function() {
    this.id = "";
    this.name = "";
    this.seats = 3;
    this.availableSeats = 3;
};
