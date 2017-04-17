'use strict';
const path = require('path');
const isWebpackDevServer = process.argv.filter(a => path.basename(a) === 'webpack-dev-server').length;
const isWatch = process.argv.filter(a => a === '--watch').length

module.exports = {
  // debug: true,
  devtool: 'eval-source-map',
  entry: './src/Main.purs',
  // entry: './src/Example.purs',

  devServer: {
    contentBase: '.',
    port: 3000,
    stats: 'errors-only'
  },

  output: {
    path: __dirname,
    pathinfo: true,
    filename: 'bundle.js'
  },

  module: {
    loaders: [
      {
        test: /\.purs$/,
        loader: 'purs-loader',
        query: {
          src: [ 'bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs' ],
          bundle: false,
          psc: 'psa', // psc
          watch: isWebpackDevServer || isWatch,
          pscBundle: 'psc-bundle',
          // pscArgs: {},
          // pscBundleArgs: {},
          // pscIde: true, // instant rebuilds using psc-ide-server (experimental)
          // pscIdeArgs: {}, // for example, to use different psc-ide-server port: {port: 4088}
          // pscIdeServerArgs: {}, // for example, to change the port { port: 4088 }
          pscPackage: false,
          pscIdeColors: true, // defaults to true if psc === 'psa'
          bundleOutput: 'output/bundle.js',
          bundleNamespace: 'PS',
          warnings: false,
          output: 'output',
        }
      },
    ]
  },

  // resolve: {
    // modulesDirectories: [ 'node_modules', 'bower_components' ],
    // extensions: [ '', '.purs', '.js']
  // },

  plugins: isWebpackDevServer || !isWatch ? [] : [
    function(){
      this.plugin('done', function(stats){
        process.stderr.write(stats.toString('errors-only'));
      });
    }
  ]
};