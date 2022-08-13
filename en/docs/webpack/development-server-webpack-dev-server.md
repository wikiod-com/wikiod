---
title: "Development server webpack-dev-server"
slug: "development-server-webpack-dev-server"
draft: false
images: []
weight: 9992
type: docs
toc: true
---

## Installation
`webpack-dev-server` can be installed via `npm`
```
npm --save-dev webpack-dev-server
```
now you can start server 
```
./node_modules/.bin/webpack-dev-server
```
To simplify usage you can add script to `package.json`
```
// package.json
{
  ...
  "scripts": {
    "start": "webpack-dev-server"
  },
  ...
}
```
now to run server you can use
```
npm run start
```

`webpack-dev-server` is configured in `webpack.config.js` file in section `devServer`.

To change server content base directory you can use option `contentBase`.
Example configuration setting root directory to `public_html` could look like
```
let path = require("path");

module.exports = {
  ...
  devServer: {
    contentBase: path.resolve(__dirname, "public_html")
  },
  ...
}
```

## Using proxy
`webpack-dev-server` can proxy some requests to others servers. This might be useful for developing API client when you want to send requests to same domain.

Proxy is configured via [`proxy`][1] parameter.

Example configuration of dev server passing requests to `/api` to other service listening on port 8080 might look like this
```
// webpack.config.js
module.exports = {
  ...
  devServer: {
    proxy: {
      "/api": {
        target: "http://localhost:8080"
      }
    }
  }
...
}
```
# rewrite

It is possible to rewrite destination path using `pathRewrite` option. 

Assuming you want to strip `/api` prefix from previous example your config might look like
```
// webpack.config.js
  ...
  devServer: {
    proxy: {
      "/api": {
        target: "http://localhost:8080",
        pathRewrite: {"^/api" : ""}
      }
    }
  }
...
```
Request `/api/user/256` will be converted to `http://localhost:8080/user/256`.

# filter
It is possible to proxy only some requests. `bypass` allows you to provide function which return value will determine if request should be proxied or not.

Assuming you only want to proxy only POST requests to `/api` and let `webpack` handle the rest your configuration might look like this

```
// webpack.config.js
  ...
  devServer: {
    proxy: {
      "/api": {
        target: "http://localhost:8080",
        bypass: function(req, res, proxyOptions) {
          if(req.method != 'POST') return false;
        }
      }
    }
  }
...
```

  [1]: https://webpack.js.org/configuration/dev-server/#devserver-proxy

