/* jshint esnext:true */

var koa = require('koa'),
    route = require('koa-route'),
    winston = require('winston'),
    json = require('koa-json'),
    routes = require('./routes'),
    
    app = module.exports = koa();

app.use(route.post('/user', routes.connectUser));
app.use(route.get('/room', routes.getPublicGameRooms));

app.on('error', function(error) {
    winston.error(error);
});

if (!module.parent) {
    winston.info('starting up app');
    app.listen(3000);
}
