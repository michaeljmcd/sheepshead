var userRepository = require('../../user/user-repository'),
    User = require('../../user/user').User,
    expect = require('expect.js'),
    database = require('../../persistence/database');

beforeEach(function() {
    database.connect(function() {
        database.clearUsers();
        database.clearRooms();
        // TODO: we need to separate databases for general use and for the
        // integration test
    });
});

describe('registerUser', function() {
    it('should return null for a null nickname', function() {
        expect(userRepository.findUserByNickname(null)).to.be(null);
    });

    it('should return null for an undefined nickname', function() {
        expect(userRepository.findUserByNickname(undefined)).to.be(null);
    });

    it('should return null for an empty nickname', function() {
        expect(userRepository.findUserByNickname('')).to.be(null);
    });

    it('should find a user after registration', function() {
        var newUser = new User();
        newUser.nickname = "bob";
        newUser.ticket = "1";

        var p = userRepository.registerUser(newUser);

        p.then(function(userFound) {
            var userFound = userRepository.findUserByNickname("bob");
            expect(userFound).not.to.be(undefined);
            expect(userFound.nickname).to.be('bob');
            expect(userFound.ticket.length).to.be(32);
            done();
        })
        .catch(function(err) {
            done(err);
        });
    });

    it('should fail to find an unregistered user', function() {
        var newUser = new User();
        newUser.nickname = "bob";
        newUser.ticket = "1";

        userRepository.registerUser(newUser);

        var userFound = userRepository.findUserByNickname("alice");
        expect(userFound).to.be(undefined);
    });

    it('should give a new nickname for a duplicate nickname', function() {
        var user1 = new User();
        user1.nickname = "bob";
        
        var user2 = new User();
        user2.nickname = "alice";

        var user3 = new User();
        user3.nickname = "bob";

        var registered1 = userRepository.registerUser(user1);
        var registered2 = userRepository.registerUser(user2);
        var registered3 = userRepository.registerUser(user3);

        expect(registered3.nickname).not.to.be('bob');
    });
});
