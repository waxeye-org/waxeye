const path = require('path');

// We only use webpack to compile a version of waxeye that exports itself to
// a global.
module.exports = {
  devtool: 'source-map',
  entry: {
    waxeye: './waxeye.ts',
  },
  output: {
    filename: './dist/waxeye.js',
    libraryTarget: 'umd',
    // `library` determines the name of the global variable
    library: '[name]'
  },
  resolve: {
    extensions: ['.ts']
  },
  module: {
    rules: [
      { test: /\.ts$/, loader: 'ts-loader' }
    ]
  }
};
