var app = require('../server'),
    winston = require('winston'),
    request = require('supertest').agent(app.listen());

winston.level = 'error';

describe('Connect User', function() {
    it('should return a user', function(done) {
        request.post('/user')
            .send({ nickname: 'bob' })
            .set('Accept', 'application/json')
            .expect(200)
            .expect(function(res) {
                var ticket = res.ticket;

                if (ticket === null || ticket === undefined) {
                    return "should have received a ticket";
                }
                else if (ticket.length !== 32) {
                    return "invalid ticket";
                }
            })
            .end(done);
    });

    it('should return a 422 code when the JSON object does not match the schema', function(done) {
        request.post('/user')
            .send({ firstName: 'bob' })
            .expect(422)
            .end(done);
    });

    it('should return a 422 when the nickname is more than 255 characters in length', function(done) {
        request.post('/user')
            .send({ nickname: 'ueyJJMC7C37imPxYM7tNrSXvYp6ketNNoDEvb1pGAhEH2Ak0F1kFUoJC1lHhuyCTAhUTNKtsrhKclnSD8l5R2f3KjmRKtLQNOJiu2a3j4UyOofBbxG2IGxoynIOp5IpuHqabzOcQY3ae7WKe5JaFvymb6z8OMMlB3FryBdV6eVp9Twpqm93j6LUcr7ceXbLUUq3ZBkFsmX6tLMA3brdDrw7FJJmcrCwt5FfkuWYk2OW0FbLAS3GiwXrghn2kiDcC' })
            .expect(422)
            .end(done);
    });
});

describe('Game Rooms', function() {
    it('should return empty when no games have yet been created', function(done) {
        request.get('/room')
            .send('Accept', 'application/json')
            .expect(200)
            .expect([])
            .end(done);
    });
});
