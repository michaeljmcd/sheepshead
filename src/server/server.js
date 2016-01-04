/* jshint esnext:true */

var koa = require('koa'),
    route = require('koa-route'),
    winston = require('winston'),
    
    app = module.exports = koa();

app.use(route.get('/', index));
app.use(route.post('/user', connectUser));

function* index() {
    this.body = 'hello world';
}

function* connectUser() {
    this.body = 'user';
}

app.on('error', function(error) {
    winston.error(error);
});

if (!module.parent) {
    winston.info('starting up app');
    app.listen(3000);
}
