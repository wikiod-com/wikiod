---
title: "Windows authentication under node.js"
slug: "windows-authentication-under-nodejs"
draft: false
images: []
weight: 9995
type: docs
toc: true
---

There are several other Active Directory APIS, such as [`activedirectory2`](https://www.npmjs.com/package/activedirectory2) and [`adldap`](https://www.npmjs.com/package/adldap).

## Using activedirectory
The example below is taken from the full docs, available [here (GitHub)](https://github.com/gheeres/node-activedirectory) or [here (NPM)](https://npmdoc.github.io/node-npmdoc-activedirectory/build/apidoc.html).
# Installation
    npm install --save activedirectory

# Usage

    // Initialize
    var ActiveDirectory = require('activedirectory');
    var config = {
        url: 'ldap://dc.domain.com',
        baseDN: 'dc=domain,dc=com'
    };
    var ad = new ActiveDirectory(config);
    var username = 'john.smith@domain.com';
    var password = 'password';
    // Authenticate
    ad.authenticate(username, password, function(err, auth) {
        if (err) {
            console.log('ERROR: '+JSON.stringify(err));
            return;
        }
        if (auth) {
            console.log('Authenticated!');
        }
        else {
            console.log('Authentication failed!');
        }
    });

