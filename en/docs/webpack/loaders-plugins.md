---
title: "Loaders & Plugins"
slug: "loaders--plugins"
draft: false
images: []
weight: 9985
type: docs
toc: true
---

Loaders and plugins make up the building blocks of Webpack.

Loaders are typically delegated to a single task and file type. They are easier to setup and usually require less boilerplate code. 

Plugins, on the other hand, have access to Webpack's internal build system via hooks, and are therefore more powerful. Plugins can modify the fully configured Webpack environment, and they can perform custom actions throughout the compilation process.

When dealing with our CSS files, for example, a loader might be used to automatically add vendor prefixes to properties, while a plugin might be used to produce a minified stylesheet in the bundler build process.

## Getting started with loaders
To begin, `npm install` the desired loaders for your project.

Inside of the configuration object that is being exported in `webpack.config.js`, a `module` property will hold all of your loaders.

    const source = `${__dirname}/client/src/`;
    
    module.exports = {
      // other settings here
      
      module: {
        loaders: [
              {
                test: /\.jsx?$/,
                include: source,
                loaders: ['babel?presets[]=es2015,presets[]=react', 'eslint']
              },
              {
                test: /\.s?css$/,
                include: source,
                loaders: ['style', 'css', 'autoprefixer', 'sass']
              }
            ]
      },
    };

In the above configuration, we're using three basic settings for our loaders:

 - **test:** This is where we bind loaders to specific extensions using RegExp. The first set of loaders is being executed on all .js and .jsx files. The second set is being executed on all .css and .scss files.
- **include:** This is the directory we want our loaders to run on. Optionally, we could just as easily use the `exclude` property to define directories we do not want included.
- **loaders:** This is a list of all the loaders we want to run on the files specified in `test` and `include`.


----------

It's important to note that loaders are executed from right to left in each loaders array, and from bottom to top in the individual definitions. The code below will execute the loaders in the following order: `sass, autoprefixer, css, style`. 

    loaders: [
      {
        test: /\.s?css$/,
        include: source,
        loaders: ['style', 'css', 'autoprefixer']
      },
      {
        test: /\.s?css$/,
        include: source,
        loaders: ['sass']
      }
    ]

This is a common source of confusion and bugs for developers who are new to webpack. For example, when using JSX and ES6 syntax, we want to lint that code, *not* lint the compiled code that is provided by our babel loader. Therefore, our eslint loader is placed to the right of our babel loader.


----------

The `-loader` suffix is optional when listing our loaders.

    loaders: ['sass']

... is equivalent to: 

    loaders: ['sass-loader']


----------
Alternatively, you can use the `loader` property (singular) along with a string separating the list of loaders by the `!` character.

    loaders: ['style', 'css']

... is equivalent to:

    loader: "style!css"

## loading typescript files
To use typescript with webpack you need `typescript` and `ts-loader` installed
```
npm --save-dev install typescript ts-loader
```
Now you can configure webpack to use typescript files
```
// webpack.config.js

module.exports = {
  ..
  resolve: {
    // .js is required for react imports.
    // .tsx is required for react tsx files.
    // .ts is optional, in case you will be importing any regular ts files.
    extensions: ['.js', '.ts', '.tsx']
  },
  module: {
    rules: [
      {
        // Set up ts-loader for .ts/.tsx files and exclude any imports from node_modules.
        test: /\.tsx?$/,
        loaders: isProduction ? ['ts-loader'] : ['react-hot-loader', 'ts-loader'],
        exclude: /node_modules/
      }
    ]
  },
  ...
};
```

