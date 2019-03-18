module.exports = {
  entry: "./src/main.ts",
  output: {
      filename: "./bundle.js",
  },

  // Enable sourcemaps for debugging webpack's output.
  devtool: "eval-source-map",

  resolve: {
      // Add '.ts' and '.tsx' as resolvable extensions.
      extensions: [".webpack.js", ".web.js", ".ts", ".tsx", ".js"]
  },

  module: {

      rules: [

          { test: /\.tsx?$/, enforce: "pre", loader: "awesome-typescript-loader" },
          // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
          { test: /\.js$/, enforce: "pre", loader: "source-map-loader" }
      ]
  },

  watchOptions: {
    aggregateTimeout: 300,
    poll: 1000,
    ignored: './node_modules/'
  }

  // Other options...
};
