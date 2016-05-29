/* jshint esnext:true */

var koa = require('koa'),
    route = require('koa-route'),
    winston = require('winston'),
    json = require('koa-json'),
    routes = require('./routes'),
    database = require('./persistence/database'),
    
    app = module.exports = koa();

database.connect(function() {
    app.use(route.post('/user', routes.connectUser));
    app.use(route.get('/room', routes.getPublicGameRooms));
    app.use(route.post('/room', routes.createPublicGameRoom));

    app.on('error', function(error) {
        winston.error(error);
    });

    if (!module.parent) {
        winston.info('starting up app');
        app.listen(process.env.PORT || 3000);
    }
});
