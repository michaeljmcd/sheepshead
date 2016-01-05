var app = require('../server');
var request = require('supertest').agent(app.listen());

describe('Index', function() {
    it('should work', function(done) {
        request.get('/')
            .expect(200)
            .expect('hello world', done);
    });
});

describe('Connect User', function() {
    it('should return a user', function(done) {
        request.post('/user')
            .expect(200, done);
    });
});
