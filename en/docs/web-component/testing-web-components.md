---
title: "Testing Web Components"
slug: "testing-web-components"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

Things to consider when we want to test our components with: Styles, Templates, Component classes.

## Webpack and Jest
[Jest](https://facebook.github.io/jest/) is used by Facebook to test all JavaScript code including React applications. One of Jest's philosophies is to provide an integrated "zero-configuration" experience. We observed that when engineers are provided with ready-to-use tools, they end up writing more tests, which in turn results in more stable and healthy code bases.

Full working example is available on GitHub as [web-components-webpack-es6-boilerplate](https://github.com/vardius/web-components-webpack-es6-boilerplate)

Jest runs tests in NodeJS enviroment with [jsdom](https://github.com/tmpvar/jsdom). The whole process is easy. Let's consider following [webpack](https://webpack.github.io/) setup, assuming our project structure looks like a following example:

    -src
        --client
        --server
    -webpack
        --config.js
    package.json

A simple directory structure designed to separate the `server render` logic from the rest. **Webpack** `config.js` file would contain following modules:

```javascript
  resolve: {
    modules: ["node_modules"],
    alias: {
      client: path.join(__dirname, "../src/client"),
      server: path.join(__dirname, "../src/server")
    },
    extensions: [".js", ".json", ".scss"]
  },
```

We can set up **Jest** to reflect our **Webpack** config.

    module.exports = {
      setupTestFrameworkScriptFile: "<rootDir>/bin/jest.js",
      mapCoverage: true,
      moduleFileExtensions: ["js", "scss", "html"],
      moduleDirectories: ["node_modules"],
      moduleNameMapper: {
        "src/(.*)$": "<rootDir>/src/$1"
      },
      transform: {
        "^.+\\.(js|html|scss)$": "<rootDir>/bin/preprocessor.js"
      },
      testMatch: ["<rootDir>/test/**/?(*.)(spec|test).js"],
      testPathIgnorePatterns: ["<rootDir>/(node_modules|bin|build)"]
    };

**Where should we save this config ?**

We can do it in the `package.json` file under `jest` key or create as in this example `jest.config.js` file in the project root.

What we want to achieve is to make sure that our `html` files are going to be imported correctly. That means by escaping them with custom `preprocessor`, as using only `babel-jest` would throw error when trying to parse non `js` files.

The other important thing here is `setupTestFrameworkScriptFile` script which actually includes `custom elements` polyfills to `jsdom`. Here is how our `preprocessor.js` looks like:

```javascript
const babelJest = require("babel-jest");

const STYLE_URLS_REGEX = /styles:\s*\[\s*((?:'|").*\s*(?:'|")).*\s*.*\]/g;
const ESCAPE_TEMPLATE_REGEX = /(\${|\`)/g;

module.exports.process = (src, path, config) => {
  if (path.endsWith(".html")) {
    src = src.replace(ESCAPE_TEMPLATE_REGEX, "\\$1");
    src = "module.exports=`" + src + "`;";
  }
  src = src.replace(STYLE_URLS_REGEX, "styles: []");

  return babelJest.process(src, path, config);
};
```

What this script does, is simple: remove style files content as we do not need/want to test it, and escape templates, when we import them for example with `require('template.html')` syntax. Then it passes down content to babel transformer.

Last important thing to do is to include `web components` `polyfills`. As by default `jsdom` does not support them yet. To do it we can simply add `setupTestFrameworkScriptFile` in our example it is `jest.js` with the following content:

```javascript
require("document-register-element/pony")(window);
```

This way we can access `web components` **API** in `jsdom`.

After setting up everything we should have structure like this:

    -bin
        --jest.js
        --preprocessor.js
    -src
        --client
        --server
    -webpack
        --config.js
    -test
    package.json
    jest.config.js

Where we keep our tests in the `test` directory and can run it with command: `yarn run jest --no-cache --config $(node jest.config.js)`.

