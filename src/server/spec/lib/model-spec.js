var model = require('../../lib/model'),
    expect = require('expect.js');

describe('User', function() {
    it('should initialize to a blank object', function() {
        var newUser = new model.User();

        expect(newUser).not.to.be(undefined);
        expect(newUser).not.to.be(null);
        expect(newUser.nickname).to.be("");
        expect(newUser.ticket).to.be("");
    });

    it('should perform a copy when passed a single instance', function() {
        var copyFromObject = { nickname: "derek webb", ticket: "12345" };
        var newUser = new model.User(copyFromObject);

        expect(newUser).not.to.be(undefined);
        expect(newUser).not.to.be(null);
        expect(newUser.nickname).to.be("derek webb");
        expect(newUser.ticket).to.be("12345");
    });

    it('should validate nicknames', function() {
        var user = new model.User();

        expect(user.isValid()).to.be(false);

        user.nickname = null;
        expect(user.isValid()).to.be(false);

        user.nickname = "alex smith";
        expect(user.isValid()).to.be(true);

        user.nickname = "ueyJJMC7C37imPxYM7tNrSXvYp6ketNNoDEvb1pGAhEH2Ak0F1kFUoJC1lHhuyCTAhUTNKtsrhKclnSD8l5R2f3KjmRKtLQNOJiu2a3j4UyOofBbxG2IGxoynIOp5IpuHqabzOcQY3ae7WKe5JaFvymb6z8OMMlB3FryBdV6eVp9Twpqm93j6LUcr7ceXbLUUq3ZBkFsmX6tLMA3brdDrw7FJJmcrCwt5FfkuWYk2OW0FbLAS3GiwXrghn2kiDcC";
        expect(user.isValid()).to.be(false);
    });
});

describe('Room', function() {
    it('should initialize with default values', function() {
        var room = new model.Room();

        expect(room).not.to.be(undefined);
        expect(room).not.to.be(null);
        expect(room.id).to.be("");
        expect(room.name).to.be("");
        expect(room.seats).to.be(3);
        expect(room.availableSeats).to.be(3);
    });

    it('should copy objects when a single argument is used', function() {
        var toCopy = { id: 71, name: "mike's game", seats: 7, availableSeats: 7, apple: true };
        var room = new model.Room(toCopy);

        expect(room).not.to.be(undefined);
        expect(room).not.to.be(null);
        expect(room.id).to.be(71);
        expect(room.name).to.be("mike's game");
        expect(room.seats).to.be(7);
        expect(room.availableSeats).to.be(7);
        expect(room.apple).to.be(undefined);
    });

    it('should validate the name is valid', function() {
        var room = new model.Room();

        room.seats = 3;
        expect(room.isValid()).to.be(true);

        room.seats = null;
        expect(room.isValid()).to.be(false);

        room.seats = 4;
        expect(room.isValid()).to.be(true);

        room.seats = 2;
        expect(room.isValid()).to.be(false);

        room.seats = 5;
        expect(room.isValid()).to.be(true);

        room.seats = 6;
        expect(room.isValid()).to.be(false);

        room.seats = 'asdf';
        expect(room.isValid()).to.be(false);

        room.seats = true;
        expect(room.isValid()).to.be(false);
    });
});
