---
title: "Creating an Angular npm library"
slug: "creating-an-angular-npm-library"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

How to publish your NgModule, written in TypeScript in npm registry.
Setting up npm project, typescript compiler, rollup and continous integration build.

## Minimal module with service class
# File structure

    /
        -src/
            awesome.service.ts
            another-awesome.service.ts
            awesome.module.ts
        -index.ts
        -tsconfig.json
        -package.json
        -rollup.config.js
        -.npmignore

# Service and module

Place your awesome work here.

**src/awesome.service.ts:**

    export class AwesomeService {
        public doSomethingAwesome(): void {
            console.log("I am so awesome!");
        }
    }

**src/awesome.module.ts:**
    
    import { NgModule } from '@angular/core'
    import { AwesomeService } from './awesome.service';
    import { AnotherAwesomeService } from './another-awesome.service';
    
    @NgModule({
        providers: [AwesomeService, AnotherAwesomeService]
    })
    export class AwesomeModule {}

Make your module and service accessible outside.

**/index.ts:**

    export { AwesomeService } from './src/awesome.service';
    export { AnotherAwesomeService } from './src/another-awesome.service';
    export { AwesomeModule } from './src/awesome.module';

# Compilation

In compilerOptions.paths you need to specify all external modules which you used in your package.

**/tsconfig.json**

    {
      "compilerOptions": {
        "baseUrl": ".",
        "declaration": true,
        "stripInternal": true,
        "experimentalDecorators": true,
        "strictNullChecks": false,
        "noImplicitAny": true,
        "module": "es2015",
        "moduleResolution": "node",
        "paths": {
          "@angular/core": ["node_modules/@angular/core"],
          "rxjs/*": ["node_modules/rxjs/*"]
        },
        "rootDir": ".",
        "outDir": "dist",
        "sourceMap": true,
        "inlineSources": true,
        "target": "es5",
        "skipLibCheck": true,
        "lib": [
          "es2015",
          "dom"
        ]
      },
      "files": [
        "index.ts"
      ],
      "angularCompilerOptions": {
        "strictMetadataEmit": true
      }
    }

Specify your externals again

**/rollup.config.js**

    export default {
        entry: 'dist/index.js',
        dest: 'dist/bundles/awesome.module.umd.js',
        sourceMap: false,
        format: 'umd',
        moduleName: 'ng.awesome.module',
        globals: {
            '@angular/core': 'ng.core',
            'rxjs': 'Rx',
            'rxjs/Observable': 'Rx',
            'rxjs/ReplaySubject': 'Rx',
            'rxjs/add/operator/map': 'Rx.Observable.prototype',
            'rxjs/add/operator/mergeMap': 'Rx.Observable.prototype',
            'rxjs/add/observable/fromEvent': 'Rx.Observable',
            'rxjs/add/observable/of': 'Rx.Observable'
        },
        external: ['@angular/core', 'rxjs']
    }

# NPM settings
Now, lets place some instructions for npm

**/package.json**

    {
      "name": "awesome-angular-module",
      "version": "1.0.4",
      "description": "Awesome angular module",
      "main": "dist/bundles/awesome.module.umd.min.js",
      "module": "dist/index.js",
      "typings": "dist/index.d.ts",
      "scripts": {
        "test": "",
        "transpile": "ngc",
        "package": "rollup -c",
        "minify": "uglifyjs dist/bundles/awesome.module.umd.js --screw-ie8 --compress --mangle --comments --output dist/bundles/awesome.module.umd.min.js",
        "build": "rimraf dist && npm run transpile && npm run package && npm run minify",
        "prepublishOnly": "npm run build"
      },
      "repository": {
        "type": "git",
        "url": "git+https://github.com/maciejtreder/awesome-angular-module.git"
      },
      "keywords": [
        "awesome",
        "angular",
        "module",
        "minimal"
      ],
      "author": "Maciej Treder <contact@maciejtreder.com>",
      "license": "MIT",
      "bugs": {
        "url": "https://github.com/maciejtreder/awesome-angular-module/issues"
      },
      "homepage": "https://github.com/maciejtreder/awesome-angular-module#readme",
      "devDependencies": {
        "@angular/compiler": "^4.0.0",
        "@angular/compiler-cli": "^4.0.0",
        "rimraf": "^2.6.1",
        "rollup": "^0.43.0",
        "typescript": "^2.3.4",
        "uglify-js": "^3.0.21"
      },
      "dependencies": {
        "@angular/core": "^4.0.0",
        "rxjs": "^5.3.0"
      }
    }


We can also specify what files, npm should ignore

**/.npmignore**

    node_modules
    npm-debug.log
    Thumbs.db
    .DS_Store
    src
    !dist/src
    plugin
    !dist/plugin
    *.ngsummary.json
    *.iml
    rollup.config.js
    tsconfig.json
    *.ts
    !*.d.ts
    .idea

# Continuous integration
Finally you can set up continuous integration build

**.travis.yml**

    language: node_js
    node_js:
    - node
    
    deploy:
      provider: npm
      email: contact@maciejtreder.com
      api_key:
        secure: <your api key>
      on:
        tags: true
        repo: maciejtreder/awesome-angular-module


Demo can be found here: https://github.com/maciejtreder/awesome-angular-module

