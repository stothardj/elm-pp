var gulp = require('gulp');
var elm = require('gulp-elm');
var gls = require('gulp-live-server');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function() {
    return gulp.src('src/*.elm')
        .pipe(elm())
        .pipe(gulp.dest('public/'));
});

gulp.task('serve', function() {
    gls.new('server.js').start();
});

gulp.task('watch', function() {
    gulp.watch('src/*.elm', ['elm'])
});

gulp.task('default', ['watch', 'serve'], function() {});
