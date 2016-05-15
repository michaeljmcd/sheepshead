var gulp = require('gulp'),
    jshint = require('gulp-jshint'),
    mocha = require('gulp-mocha'),
    pandoc = require('gulp-pandoc');

gulp.task('generate documentation', function() {
    gulp.src('doc/specification*.md')
        .pipe(pandoc({
            from: 'markdown',
            to: 'html5',
            ext: '.html',
            args: ['--smart','--standalone','--toc']
        }))
        .pipe(gulp.dest('doc/html/'));
});

gulp.task('lint server module', function() {
    gulp.src(['src/server/*.js', 'src/server/lib/*.js', 'src/server/spec/**/*.js'])
        .pipe(jshint())
        .pipe(jshint.reporter('default'));
});

gulp.task('test', function() {
    gulp.src('src/server/spec/**/*.js', {read: false})
        .pipe(mocha())
        .once('error', function() { process.exit(1); })
        .once('end', function() { process.exit(); });
});

gulp.task('default', ['lint server module', 'test']);
