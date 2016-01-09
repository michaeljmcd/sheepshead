/* jshint esnext:true */

var koa = require('koa'),
    route = require('koa-route'),
    winston = require('winston'),
    json = require('koa-json'),
    routes = require('./routes'),
    
    app = module.exports = koa();

app.use(route.get('/', routes.index));
app.use(route.post('/user', routes.connectUser));

app.on('error', function(error) {
    winston.error(error);
});

if (!module.parent) {
    winston.info('starting up app');
    app.listen(3000);
}
