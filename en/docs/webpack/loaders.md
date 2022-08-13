---
title: "Loaders"
slug: "loaders"
draft: false
images: []
weight: 9975
type: docs
toc: true
---

Webpack loaders can be configured as "preLoaders", "loaders" and "postLoaders".  Although they don't have to be, configurations which use linting or other imperative or serial operations can take advantage of these build stages in the pipeline.  

The key to understanding loaders and their use is that Webpack will run each module in the require graph through the loader system.  Following the example above, this means that as Webpack begins crawling through the imports of your application, it will identify the files required and using a simple regex, will determine which file or file type requires what loader or series of loaders.  

Above you can see that all ".js" or ".jsx" files will be es-linted by the [eslint-loader][1] in the preLoader phase.  Other `js|x` file types will also be run through the [babel-loader][2] in the main loader phase.  Also, in the same phase, any `.scss` files will be loaded into the [sass-loader][3].  This allows you to import Sass files in your JS modules and have them be output to the resulting JS bundle or even another separate standalone CSS bundle (using a [plugin][4]).  

**Note:**
Importing `.scss` files will only work with Webpack and an appropriate loader.  Node will not understand this kind of import without a pre-processor or transpiler.  

Also of note in the `.scss` example is the ability to "chain" loaders using the `!` exclamation mark as a "pipe" between different loaders.  The example above pipes the output of the "sass-loader" into the "css-loader" and finally to the "style-loader" This could also be performed with an array of `loaders: ['style-loader', 'css-loader', 'sass-loader']`.  Different options are also available to inline loader definitions and follow the query [parameter][5] syntax commonly found in URLs. 

**See also:**
https://webpack.github.io/docs/loaders.html


  [1]: https://github.com/MoOx/eslint-loader
  [2]: https://github.com/babel/babel-loader
  [3]: https://github.com/jtangelder/sass-loader
  [4]: https://github.com/webpack/extract-text-webpack-plugin
  [5]: http://webpack.github.io/docs/loaders.html#parameters

## Config using preLoader for eslint, babel for jsx and css loader chaining.
The following configuration can be used as a base config for bundling up your project as a library.  Notice how the module config contains a list of preLoaders and loaders.

    // webpack.config.js
    
    var path = require('path');

    module.exports = {
        entry: path.join(__dirname, '..', 'src/index.js'),
        output: {
            path: path.join(__dirname, '..', '/lib'),
            filename: outputFile,
            library: 'myCoolBundle.js',
            libraryTarget: 'umd',
            umdNamedDefine: true
        },
        module: {
            preLoaders: [
                {
                    test: /(\.jsx|\.js)$/,
                    loader: "eslint-loader",
                    exclude: /node_modules/,
                }
            ],
            loaders: [
                {
                    test: /(\.jsx|\.js)$/,
                    loader: ['babel'],
                    exclude: /(node_modules)/,
                    include: path.join(__dirname, '..'),
                    query: {
                        presets: [ 'es2015', 'react']
                    }
                },
                {
                    test: /\.scss$/,
                    loaders: ["style-loader", "css-loader!sass-loader"]
                }
            ]
        },
        resolve: {
            root: path.resolve(__dirname, '..', './src'),
            extensions: ['', '.js', '.jsx', '.scss'],
            fallback: path.join(__dirname, '../node_modules')
        },
        eslint: {
            configFile: path.resolve(__dirname, '..', '.eslintrc'),
        }
    };

