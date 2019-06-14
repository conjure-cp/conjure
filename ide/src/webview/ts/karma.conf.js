// Karma configuration
// Generated on Thu Mar 14 2019 16:58:44 GMT+0000 (Greenwich Mean Time)

module.exports = function(config) {
  config.set({
    singleRun: true,
    // autoWatch: true,

    frameworks: ["jasmine", "karma-typescript"],
    files: [
      "src/**/*.ts", // *.tsx for React Jsx
      "test/**/*.ts", // *.tsx for React Jsx
      "src/fake.js",
      "https://code.jquery.com/jquery-3.3.1.min.js",
      "https://d3js.org/d3.v3.min.js",
      "https://cdn.jsdelivr.net/npm/jspanel4@4.2.1/dist/jspanel.js",
      "https://cdnjs.cloudflare.com/ajax/libs/mousetrap/1.6.2/mousetrap.min.js"
    ],
    preprocessors: {
      "**/*.ts": "karma-typescript" // *.tsx for React Jsx
    },
    reporters: ["progress", "karma-typescript"],
    browsers: ["ChromeHeadless"],
    // browsers: ["Chrome"],

    karmaTypescriptConfig: {
      tsconfig: "./tsconfig.json"
    }
  });
};

// module.exports = function(config) {
//   config.set({

//     // base path that will be used to resolve all patterns (eg. files, exclude)
//     basePath: '',

//     // frameworks to use
//     // available frameworks: https://npmjs.org/browse/keyword/karma-adapter
//     frameworks: ['jasmine'],

//     // list of files / patterns to load in the browser
//     files: [
//       './test/*.spec.ts'
//     ],

//     // list of files / patterns to exclude
//     exclude: [
//     ],

//     // preprocess matching files before serving them to the browser
//     // available preprocessors: https://npmjs.org/browse/keyword/karma-preprocessor
//     preprocessors: {
//     },

//     // test results reporter to use
//     // possible values: 'dots', 'progress'
//     // available reporters: https://npmjs.org/browse/keyword/karma-reporter
//     reporters: ['progress'],

//     // web server port
//     port: 9876,

//     // enable / disable colors in the output (reporters and logs)
//     colors: true,

//     // level of logging
//     // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
//     logLevel: config.LOG_INFO,

//     // enable / disable watching file and executing tests whenever any file changes
//     autoWatch: true,

//     // start these browsers
//     // available browser launchers: https://npmjs.org/browse/keyword/karma-launcher
//     browsers: ['Chrome'],

//     // Continuous Integration mode
//     // if true, Karma captures browsers, runs the tests and exits
//     singleRun: false,

//     // Concurrency level
//     // how many browser should be started simultaneous
//     concurrency: Infinity
//   })
// }
