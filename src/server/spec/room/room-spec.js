var model = require('../../room/room'),
    expect = require('expect.js');

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
