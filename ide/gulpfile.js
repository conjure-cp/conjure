var browserify = require('browserify');
var gulp = require('gulp');
var source = require('vinyl-source-stream');

let mainPath ='./src/webview/scripts/main.js';

gulp.task('browserify', function() {
    return browserify(mainPath, {"debug": true})
        .bundle()
        //Pass desired output filename to vinyl-source-stream
        .pipe(source('bundle.js'))
        // Start piping stream to tasks!
        .pipe(gulp.dest('./out/'));
});

gulp.task('watch:js', function(){
    gulp.watch('./src/webview/scripts/**', gulp.series('browserify'));
})