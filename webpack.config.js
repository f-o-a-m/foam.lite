'use strict';

const path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const BrotliPlugin = require('brotli-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const TerserPlugin = require("terser-webpack-plugin");
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin')
const webpack = require('webpack');
const isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
const isWatch = process.argv.some(a => a === '--watch');
const isDev = isWebpackDevServer || isWatch || process.env.NODE_ENV !== 'production';

console.log(isWebpackDevServer, isWatch, process.env.NODE_ENV);

if (isDev) {
  console.log("Looks like we're in dev mode, not including optimizations");
} else {
  console.log("Looks like we're in prod mode. Set a course for Deep Space 9; Warp six, engage!");
}

let plugins = [];
const defines = {
  'process.env.URL': JSON.stringify(process.env.URL),
  'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV),
  'process.env.FALLBACK_HTTP_PROVIDER': JSON.stringify(process.env.FALLBACK_HTTP_PROVIDER)
};

console.log("exporting the following defines: ", JSON.stringify(defines));

plugins.push(
  new webpack.DefinePlugin(defines)
);
plugins.push(new webpack.LoaderOptionsPlugin({
  debug: true
}));
plugins.push(new HtmlWebpackPlugin({
  title: 'FOAM Lite',
  template: 'index.html',
  inject: true  // See stackoverflow.com/a/38292765/3067181
}));
if (!isDev) {
  plugins.push(new MiniCssExtractPlugin());
  plugins.push(new BrotliPlugin({
    asset: '[path].br[query]',
    test: /\.(js|css|html|svg)$/,
    threshold: 10240,
    minRatio: 0.8
  }));
}
if (isDev) {
  plugins.push(function() {
    this.plugin('done', function(stats){
      process.stderr.write(stats.toString('errors-only'));
    });
  });
}


module.exports = {
  devtool: '', //'eval-source-map',

  devServer: {
    host: '0.0.0.0',
    contentBase: path.resolve(__dirname, 'dist'),
    port: 4008,
    stats: 'errors-only'
  },

  entry: './app/index.js',

  output: {
    path: path.resolve(__dirname, 'dist'),
    filename: 'bundle.js'
  },

  module: {
    rules: [
      {
        test: /\.purs$/,
        use: [
          {
            loader: 'purs-loader',
            options: {
              src: [
                'app/**/*.purs'
              ],
              spago: true,
              watch: isWebpackDevServer || isWatch,
              pscIde: true
            }
          }
        ]
      },
      {
        test: /\.(sa|sc|c)ss$/,
        use: [ (isDev ? 'style-loader' : MiniCssExtractPlugin.loader), 'css-loader', 'postcss-loader' ],
      },
      {
          test: /\.(glsl|vs|fs)$/,
          loader: 'shader-loader',
      },
      {
        test: /\.(png|jpg|gif)$/i,
        use: [
          {
            loader: 'url-loader',
            options: {
              limit: 8192,
            },
          },
        ],
      },
    ]
  },
  resolveLoader: {
    modules: [
      path.join(__dirname, 'node_modules')
    ]
  },
  resolve: {
    alias: { "~": path.resolve(__dirname, "app/modules") },
    modules: [ 'node_modules' ],
    extensions: [ '.purs', '.js' ]
  },
  optimization: {
    splitChunks: {
      cacheGroups: {
        commons: {
          test: /[\\/]node_modules[\\/]/,
          name: 'vendors',
          chunks: 'all'
        },
        styles: {
          name: 'styles',
          test: /\.css$/,
          chunks: 'all',
          enforce: true,
        }
      }
    },
    minimize: !isDev,
    minimizer: [
      new TerserPlugin(),
      new OptimizeCSSAssetsPlugin({
        // cssnano configuration
        cssProcessorPluginOptions: {
            preset: [
              'default',
              {
                discardComments: {
                  removeAll: true
                }
              }
            ],
        },
      })
    ],
  },
  plugins
};
