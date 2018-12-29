const path = require('path')

module.exports = {
  // mode: 'development',
  // devtool: 'inline-source-map',
  entry: './scripts/main.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, '../../out'),
  },
  watchOptions: {
    aggregateTimeout: 300,
    poll: 1000,
    ignored: '**/node_modules/**'
  },
  // devtool: 'inline-source-map'
}
