module.exports = function(grunt) {

    grunt.initConfig({
        sass: {
            dist: {
                files: {
                    'css/sass.css': 'css/main.scss',
                }
            }
        },
        autoprefixer: {
            dist: {
                files: {
                    'css/main.css': 'css/sass.css'
                }
            }
        },
        watch: {
            styles: {
                files: ['css/main.scss'],
                tasks: ['sass','autoprefixer']
            }
        }
    });

    grunt.loadNpmTasks('grunt-contrib-sass');
    grunt.loadNpmTasks('grunt-autoprefixer');
    grunt.loadNpmTasks('grunt-contrib-watch');

    // Notify when a watched file is modified
    grunt.event.on('watch', function(action, filepath, target) {
        grunt.log.writeln(target + ': ' + filepath + ' has ' + action);
    });
};
