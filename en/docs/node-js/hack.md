---
title: "Hack"
slug: "hack"
draft: false
images: []
weight: 9987
type: docs
toc: true
---

## Add new extensions to require()
You can add new extensions to `require()` by extending `require.extensions`.

For a **XML** example:

```
// Add .xml for require()
require.extensions['.xml'] = (module, filename) => {
    const fs = require('fs')
    const xml2js = require('xml2js')

    module.exports = (callback) => {
        // Read required file.
        fs.readFile(filename, 'utf8', (err, data) => {
            if (err) {
                callback(err)
                return
            }
            // Parse it.
            xml2js.parseString(data, (err, result) => {
                callback(null, result)
            })
        })
    }
}
```

If the content of `hello.xml` is following:
```
<?xml version="1.0" encoding="UTF-8"?>
<foo>
    <bar>baz</bar>
    <qux />
</foo>
```

You can read and parse it through `require()`:
```
require('./hello')((err, xml) {
    if (err)
        throw err;
    console.log(err);
})
```

It prints `{ foo: { bar: [ 'baz' ], qux: [ '' ] } }`.


