---
title: "DllPlugin and DllReferencePlugin"
slug: "dllplugin-and-dllreferenceplugin"
draft: false
images: []
weight: 9912
type: docs
toc: true
---

The Dll and DllReference plugins allow the code to be split in multiple bundles in a way the bundles can be compiled independently.

It is possible to build "vendor" scripts in a library that does not need to be compiled often (ex: React, jQuery, Bootstrap, Fontawesome...) and reference it in your app bundle that will need those scripts.

The application bundle, the one that is constantly going to be changed, will be in a separate configuration just referencing a already built "vendor" bundle.

## Syntax
- new webpack.DllPlugin({ path: '[name]-manifest.json', name: '[name]_[hash]' })
- new webpack.DllReferencePlugin({ context: __dirname, manifest: require('./packname-manifest.json') })

## Vendor configuration (DllPlugin)
Note: The `output.library` and `name` (in DllPlugin) must be the same.

    const path = require('path');
    const webpack = require('webpack');
    const ExtractTextPlugin = require('extract-text-webpack-plugin');
    const extractCSS = new ExtractTextPlugin('vendor.css');
    const isDevelopment = process.env.NODE_ENV !== 'production';
    
    module.exports = {
      resolve: {
        extensions: ['.js'],
      },
      module: {
        rules: [
          { test: /\.(png|woff|woff2|eot|ttf|svg)$/, loader: 'url-loader?limit=100000' },
          { test: /\.s?css$/i, loader: extractCSS.extract(['css-loader?minimize', 'sass-loader']) },
          { test: /\.json$/, loader: 'json-loader' },
        ],
      },
      entry: {
        vendor: [
          'babel-polyfill',
          'font-awesome/scss/font-awesome.scss',
          'bootstrap/scss/bootstrap.scss',
          'jquery',
          'history',
          'react',
          'react-dom',
          'redux',
          'react-redux',
          'react-router',
          'react-router-dom',
          'react-router-redux',
          'redux-thunk',
        ],
      },
      output: {
        path: path.resolve('./dist'),
        filename: '[name].js',
        library: '[name]_[hash]',
      },
      plugins: [
        extractCSS,
        new webpack.DllPlugin({
          path: path.join(__dirname, 'dist', '[name]-manifest.json'),
          name: '[name]_[hash]',
        })
      ].concat(isDevelopment ? [] : [
        new webpack.optimize.UglifyJsPlugin({
          beautify: false,
          comments: false,
        }),
      ]),
    };

## Referencing a Dll Bundle (DllReferencePlugin)
Note: `manifest` (in DllReferencePlugin) should reference `path` (defined in DllPlugin)

    const webpack = require('webpack');
    const path = require('path');
    const isDevelopment = process.env.NODE_ENV !== 'production';
    
    const ExtractTextPlugin = require('extract-text-webpack-plugin');
    const extractCSS = new ExtractTextPlugin('app.css');
    
    const merge = require('extendify')({ isDeep: true, arrays: 'concat' });
    
    module.exports = merge({
      context: __dirname,
      entry: {
        app: (isDevelopment ? ['webpack-hot-middleware/client'] : []).concat(['./src/']),
      },
      output: {
        path: path.resolve('./dist'),
        publicPath: '/static',
        filename: '[name].js',
      },
      resolve: {
        extensions: ['.js', '.ts', '.tsx'],
      },
      module: {
        loaders: [
          {
            test: /\.tsx?$/,
            loader: 'babel-loader!awesome-typescript-loader?forkChecker=true',
            include: /src|spec/,
          },
          {
            test: /\.s?css$/,
            loader: extractCSS.extract(['css-loader?minimize', 'sass-loader']),
            include: /src/,
          },
        ],
      },
      plugins: [
        new webpack.DllReferencePlugin({
          context: __dirname,
          manifest: require('./dist/vendor-manifest.json'),
        }),
        new webpack.DefinePlugin({
          'process.env': {
            'ENV': JSON.stringify(process.env.NODE_ENV),
          },
        }),
        extractCSS,
      ],
    }, isDevelopment ? require('./webpack.config.development') : require('./webpack.config.production'));

