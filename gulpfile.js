var gulp = require('gulp'),
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

gulp.task('default', ['generate documentation']);
