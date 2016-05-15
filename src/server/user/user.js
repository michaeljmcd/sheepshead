'use strict';
var util = require('../util/utility-functions');

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

module.exports.User = User;
