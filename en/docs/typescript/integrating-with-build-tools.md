---
title: "Integrating with Build Tools"
slug: "integrating-with-build-tools"
draft: false
images: []
weight: 9914
type: docs
toc: true
---

For more information you can go on official web page [typescript integrating with build tools](http://www.typescriptlang.org/docs/handbook/integrating-with-build-tools.html)

## Browserify
# Install

    npm install tsify

# Using Command Line Interface

    browserify main.ts -p [ tsify --noImplicitAny ] > bundle.js

# Using API

    var browserify = require("browserify");
    var tsify = require("tsify");

    browserify()
      .add("main.ts")
      .plugin("tsify", { noImplicitAny: true })
      .bundle()
      .pipe(process.stdout);

More details: [smrq/tsify](https://github.com/smrq/tsify)

## Webpack
# Install

    npm install ts-loader --save-dev

# Basic webpack.config.js

## webpack 2.x, 3.x

    module.exports = {
        resolve: {
            extensions: ['.ts', '.tsx', '.js']
        },
        module: {
            rules: [
                {
                    // Set up ts-loader for .ts/.tsx files and exclude any imports from node_modules.
                    test: /\.tsx?$/,
                    loaders: ['ts-loader'],
                    exclude: /node_modules/
                }
            ]
        },
        entry: [
            // Set index.tsx as application entry point.
            './index.tsx'
        ],
        output: {
          filename: "bundle.js"
        }
    };

## webpack 1.x

```javascript
module.exports = {
    entry: "./src/index.tsx",
    output: {
        filename: "bundle.js"
    },
    resolve: {
        // Add '.ts' and '.tsx' as a resolvable extension.
        extensions: ["", ".webpack.js", ".web.js", ".ts", ".tsx", ".js"]
    },
    module: {
        loaders: [
            // all files with a '.ts' or '.tsx' extension will be handled by 'ts-loader'
            { test: /\.ts(x)?$/, loader: "ts-loader", exclude: /node_modules/ }
        ]
    }
}
```

See more details on [ts-loader here](https://www.npmjs.com/package/ts-loader).

Alternatives:

* [awesome-typescript-loader](https://www.npmjs.com/package/awesome-typescript-loader)

## Grunt
# Install

    npm install grunt-ts

# Basic Gruntfile.js

```javascript
module.exports = function(grunt) {
    grunt.initConfig({
        ts: {
            default : {
                src: ["**/*.ts", "!node_modules/**/*.ts"]
            }
        }
    });
    grunt.loadNpmTasks("grunt-ts");
    grunt.registerTask("default", ["ts"]);
};
```
More details: [TypeStrong/grunt-ts](https://github.com/TypeStrong/grunt-ts)

## Gulp
# Install

    npm install gulp-typescript

# Basic gulpfile.js

```javascript
var gulp = require("gulp");
var ts = require("gulp-typescript");

gulp.task("default", function () {
    var tsResult = gulp.src("src/*.ts")
        .pipe(ts({
              noImplicitAny: true,
              out: "output.js"
        }));
    return tsResult.js.pipe(gulp.dest("built/local"));
});
```

# gulpfile.js using an existing tsconfig.json

```javascript
var gulp = require("gulp");
var ts = require("gulp-typescript");

var tsProject = ts.createProject('tsconfig.json', {
    noImplicitAny: true // You can add and overwrite parameters here
});

gulp.task("default", function () {
    var tsResult = tsProject.src()
        .pipe(tsProject());
    return tsResult.js.pipe(gulp.dest('release'));
});
```

More details: [ivogabe/gulp-typescript](https://github.com/ivogabe/gulp-typescript)

## MSBuild
Update project file to include locally installed `Microsoft.TypeScript.Default.props` (at the top) and `Microsoft.TypeScript.targets` (at the bottom) files:

```xml
<?xml version="1.0" encoding="utf-8"?>
<Project ToolsVersion="4.0" DefaultTargets="Build" xmlns="http://schemas.microsoft.com/developer/msbuild/2003">
  <!-- Include default props at the bottom -->
  <Import
      Project="$(MSBuildExtensionsPath32)\Microsoft\VisualStudio\v$(VisualStudioVersion)\TypeScript\Microsoft.TypeScript.Default.props"
      Condition="Exists('$(MSBuildExtensionsPath32)\Microsoft\VisualStudio\v$(VisualStudioVersion)\TypeScript\Microsoft.TypeScript.Default.props')" />

  <!-- TypeScript configurations go here -->
  <PropertyGroup Condition="'$(Configuration)' == 'Debug'">
    <TypeScriptRemoveComments>false</TypeScriptRemoveComments>
    <TypeScriptSourceMap>true</TypeScriptSourceMap>
  </PropertyGroup>
  <PropertyGroup Condition="'$(Configuration)' == 'Release'">
    <TypeScriptRemoveComments>true</TypeScriptRemoveComments>
    <TypeScriptSourceMap>false</TypeScriptSourceMap>
  </PropertyGroup>

  <!-- Include default targets at the bottom -->
  <Import
      Project="$(MSBuildExtensionsPath32)\Microsoft\VisualStudio\v$(VisualStudioVersion)\TypeScript\Microsoft.TypeScript.targets"
      Condition="Exists('$(MSBuildExtensionsPath32)\Microsoft\VisualStudio\v$(VisualStudioVersion)\TypeScript\Microsoft.TypeScript.targets')" />
</Project>
```

More details about defining MSBuild compiler options: [Setting Compiler Options in MSBuild projects](http://www.typescriptlang.org/docs/handbook/compiler-options-in-msbuild.html)

## NuGet
* Right-Click -> Manage NuGet Packages
* Search for `Microsoft.TypeScript.MSBuild`
* Hit `Install`
* When install is complete, rebuild!

More details can be found at [Package Manager Dialog](http://docs.nuget.org/Consume/Package-Manager-Dialog) and [using nightly builds with NuGet](https://github.com/Microsoft/TypeScript/wiki/Nightly-drops#using-nuget-with-msbuild)

## Install and configure webpack + loaders
Installation

    npm install -D webpack typescript ts-loader

webpack.config.js

    module.exports = {
      entry: {
        app: ['./src/'],
      },
      output: {
        path: __dirname,
        filename: './dist/[name].js',
      },
      resolve: {
        extensions: ['', '.js', '.ts'],
      },
      module: {
        loaders: [{
          test: /\.ts(x)$/, loaders: ['ts-loader'], exclude: /node_modules/
        }],
      }
    };

