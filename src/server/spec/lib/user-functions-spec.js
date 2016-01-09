var userLogic = require('../../lib/user-functions'),
    User = require('../../lib/model').User,
    expect = require('expect.js');

describe('registerUser', function() {
    it('should return null for a null nickname', function() {
        expect(userLogic.findUserByNickname(null)).to.be(null);
    });

    it('should return null for an undefined nickname', function() {
        expect(userLogic.findUserByNickname(undefined)).to.be(null);
    });

    it('should return null for an empty nickname', function() {
        expect(userLogic.findUserByNickname('')).to.be(null);
    });

    it('should find a user after registration', function() {
        var newUser = new User();
        newUser.nickname = "bob";
        newUser.ticket = "1";

        userLogic.registerUser(newUser);

        var userFound = userLogic.findUserByNickname("bob");
        expect(userFound).not.to.be(undefined);
        expect(userFound.nickname).to.be('bob');
        expect(userFound.ticket.length).to.be(32);
    });

    it('should fail to find an unregistered user', function() {
        var newUser = new User();
        newUser.nickname = "bob";
        newUser.ticket = "1";

        userLogic.registerUser(newUser);

        var userFound = userLogic.findUserByNickname("alice");
        expect(userFound).to.be(undefined);
    });
});
