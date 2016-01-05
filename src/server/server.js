/* jshint esnext:true */

var koa = require('koa'),
    route = require('koa-route'),
    winston = require('winston'),
    json = require('koa-json'),
    User = require('./lib/model').User,
    
    app = module.exports = koa();

app.use(route.get('/', index));
app.use(route.post('/user', connectUser));

function* index() {
    this.body = 'hello world';
}

function* connectUser() {
    var user = new User();
    this.body = user;
}

app.on('error', function(error) {
    winston.error(error);
});

if (!module.parent) {
    winston.info('starting up app');
    app.listen(3000);
}
