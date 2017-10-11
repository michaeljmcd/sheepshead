var User = require('./user/user').User,
    repo = require('./user/user-repository'),
    database = require('./persistence/database');

database.connect(function() {
    var bob = new User();
    bob.nickname = "bob";
    bob.ticket = "1";

    console.log(bob);
    repo.registerUser(bob);

    var user = repo.findUserByNickname('bob');
    console.log(user);
    user = repo.findUserByNickname('bob');
    console.log(user);
    user = repo.findUserByNickname('bob');
    console.log(user);

    /*
    var iter = repo.registerUser(bob);

    console.log(iter.next().value);
    console.log(iter.next().value);
    console.log(iter.next().value);
    */

    /*
    for(var i of repo.registerUser(bob)) {
        console.log(i);
    }
    */

    database.disconnect();

    return;
});
